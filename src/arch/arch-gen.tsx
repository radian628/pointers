import { OperandTypes, TypeOperand } from "./arch";

export type OperandVariant = {
  id: number;
};

type OperandEnum = {
  type: "enum";
  variants: readonly OperandVariant[];
};

type OperandNumber = {
  type: "number";
  bytes: number;
};

export type OperandSpec = OperandEnum | OperandNumber;

export type MapOperandToValue<Op extends OperandSpec> = Op extends OperandEnum
  ? { type: "enum"; variant: number }
  : { type: "float"; number: number } | { type: "int"; number: BigInt };

export type MapOperandsToValue<Ops extends readonly OperandSpec[]> =
  Ops extends readonly [
    infer First extends OperandSpec,
    ...infer Rest extends readonly OperandSpec[]
  ]
    ? [MapOperandToValue<First>, ...MapOperandsToValue<Rest>]
    : [];

export type InstructionSpec<Operands extends readonly OperandSpec[]> = {
  operands: Operands;
};

// info for the memory layout of a single instruction
export type OpcodeInfo = {
  // opcode for this instruction
  opcode: number;

  // size of this instruction in bytes
  size: number;

  // operand byte offsets
  operands: {
    offset: number;
  }[];
};

// maps instructions to opcodes
export type OpcodeMapping<
  Instructions extends Record<string, InstructionSpec<readonly OperandSpec[]>>
> = {
  [K in keyof Instructions]: OpcodeInfo;
};

function roundup(x: number, g: number) {
  return Math.ceil(x / g) * g;
}

function getInstructionSizingInfo(
  instr: InstructionSpec<readonly OperandSpec[]>
): {
  size: number;
  operands: {
    offset: number;
  }[];
} {
  let offset = 1;

  const operands: { offset: number }[] = [];

  for (const op of instr.operands) {
    const sizeofThisOperand =
      op.type === "number"
        ? op.bytes
        : roundup(Math.log2(op.variants.length), 8);
    operands.push({
      offset,
    });
    offset += sizeofThisOperand;
  }

  return {
    size: offset,
    operands,
  };
}

export class AssemblyLanguageSpecification<
  Instructions extends Record<string, InstructionSpec<readonly OperandSpec[]>>
> {
  generator(): AssemblyGenerator<Instructions> {
    return new AssemblyGenerator(this);
  }

  instructions: Instructions;

  opcodes: OpcodeMapping<Instructions>;

  littleEndian: boolean;

  constructor(instructions: Instructions, littleEndian: boolean) {
    this.instructions = instructions;
    this.littleEndian = littleEndian;

    let opcodeIndex = 0;

    this.opcodes = Object.fromEntries(
      Object.entries(this.instructions).map(([name, instr]) => {
        return [
          name,
          {
            opcode: opcodeIndex++,
            ...getInstructionSizingInfo(instr),
          },
        ] as [keyof Instructions, OpcodeInfo];
      })
    ) as OpcodeMapping<Instructions>;
  }
}

type OperandValue =
  | { type: "enum"; variant: number }
  | { type: "float"; number: number }
  | { type: "int"; number: bigint };

const insertOperandStagingBuffer = new DataView(new ArrayBuffer(8));

function insertOperandValue(
  dst: DataView,
  littleEndian: boolean,
  offset: number,
  size: number,
  value: OperandValue
) {
  let insertIndex = 0;

  // stuff and format values into the staging buffer
  switch (value.type) {
    case "int":
      insertOperandStagingBuffer.setBigInt64(0, value.number, littleEndian);
      insertIndex = littleEndian ? 0 : 8 - size;
      break;
    case "float":
      if (size === 8) {
        insertOperandStagingBuffer.setFloat64(0, value.number, littleEndian);
      } else if (size === 4) {
        insertOperandStagingBuffer.setFloat32(0, value.number, littleEndian);
      } else {
        throw new Error(`Assembler Error: Invalid float size '${size}'`);
      }
      break;
    case "enum":
      insertOperandStagingBuffer.setBigInt64(
        0,
        BigInt(value.variant),
        littleEndian
      );
      insertIndex = littleEndian ? 0 : 8 - size;
      break;
  }

  // set bytes in the destination buffer
  for (let i = 0; i < size; i++) {
    dst.setUint8(
      offset + i,
      insertOperandStagingBuffer.getUint8(insertIndex + i)
    );
  }
}

function copyArrayBuffer(opts: { dst: ArrayBuffer; src: ArrayBuffer }) {
  const dst = new Uint8Array(opts.dst);
  const src = new Uint8Array(opts.src);

  for (let i = 0; i < src.length; i++) {
    this.dst[i] = this.src[i];
  }
}

type PropertiesOfType<Obj extends object, Prop> = {
  [K in keyof Obj]-?: Obj[K] extends Prop ? Obj[K] : never;
};

type T = PropertiesOfType<{ a: number; b: string }, number>;

// collect a linked list into an array
function collect<
  T extends Record<any, any>,
  K extends keyof PropertiesOfType<T, T | undefined>
>(obj: T | undefined, key: K): T[] {
  if (!obj) return [];
  return [...collect(obj[key], key), obj];
}

export class AssemblyGenerator<
  Instructions extends Record<string, InstructionSpec<readonly OperandSpec[]>>
> {
  spec: AssemblyLanguageSpecification<Instructions>;
  prev?: AssemblyGenerator<Instructions>;
  data?: ArrayBuffer;

  constructor(
    spec: AssemblyLanguageSpecification<Instructions>,
    prev?: AssemblyGenerator<Instructions>
  ) {
    this.spec = spec;
    this.prev = prev;

    // @ts-expect-error I can't typecheck this lol
    this.emit = Object.entries(spec.instructions).map(([name, instr]) => {
      return [
        instr,
        (...operands: OperandValue[]) => {
          const opcode = this.spec.opcodes[name];
          const nextData = new ArrayBuffer(opcode.size);
          const nextDataView = new DataView(nextData);

          for (let i = 0; i < operands.length; i++) {
            const operandLayout = opcode.operands[i];
            insertOperandValue(
              nextDataView,
              true,
              operandLayout.offset,
              (opcode.operands[i + 1]?.offset ?? opcode.size) -
                operandLayout.offset,
              operands[i]
            );
          }

          const nextgen = new AssemblyGenerator(this.spec, this);
          nextgen.data = nextData;
          return nextgen;
        },
      ];
    });
  }

  _size: number;

  getProgramCodeSize() {
    this._size =
      (this.prev?.getProgramCodeSize() ?? 0) + (this.data?.byteLength ?? 0);
    return this._size;
  }

  createProgramCode() {
    const generators = collect(this, "prev");
    const programCodeSize = this.getProgramCodeSize();
    const fullBuffer = new ArrayBuffer(programCodeSize);
    let pos = 0;
    for (const gen of generators) {
      if (!gen.data) continue;
      copyArrayBuffer({
        src: gen.data,
        dst: fullBuffer.slice(pos),
      });
      pos += gen.data.byteLength;
    }
  }

  emit: {
    [K in keyof Instructions]: (
      ...operands: MapOperandsToValue<Instructions[K]["operands"]>
    ) => AssemblyGenerator<Instructions>;
  };
}
