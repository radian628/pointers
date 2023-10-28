import {
  AssemblyLanguageSpecification,
  InstructionSpec,
  OperandSpec,
  OperandVariant,
} from "./arch-gen";

export enum OperandTypes {
  Int8,
  Int16,
  Int32,
  Int64,
  Uint8,
  Uint16,
  Uint32,
  Uint64,
  Float32,
  Float64,
}

const IntegerTypeOperand = [
  {
    id: OperandTypes.Int8,
    label: "int8",
    desc: "Signed 8-bit integer.",
  },
  {
    id: OperandTypes.Int16,
    label: "int16",
    desc: "Signed 16-bit integer.",
  },
  {
    id: OperandTypes.Int32,
    label: "int32",
    desc: "Signed 32-bit integer.",
  },
  {
    id: OperandTypes.Int64,
    label: "int64",
    desc: "Signed 64-bit integer.",
  },
  {
    id: OperandTypes.Uint8,
    label: "uint8",
    desc: "Unsigned 8-bit integer.",
  },
  {
    id: OperandTypes.Uint16,
    label: "uint16",
    desc: "Unsigned 16-bit integer.",
  },
  {
    id: OperandTypes.Uint32,
    label: "uint32",
    desc: "Unsigned 32-bit integer.",
  },
  {
    id: OperandTypes.Uint64,
    label: "uint64",
    desc: "Unsigned 64-bit integer.",
  },
] as const;

export const TypeOperand = [
  ...IntegerTypeOperand,
  {
    id: OperandTypes.Float32,
    label: "float32",
    desc: "32-bit floating-point number (float).",
  },
  {
    id: OperandTypes.Float64,
    label: "float64",
    desc: "64-bit floating-point number (double).",
  },
] as const;

export const arch = new AssemblyLanguageSpecification(
  {
    // arithmetic
    add: binopInstr(TypeOperand),
    sub: binopInstr(TypeOperand),
    mul: binopInstr(TypeOperand),
    div: binopInstr(TypeOperand),
    mod: binopInstr(TypeOperand),

    // logical
    and: binopInstr(TypeOperand),
    or: binopInstr(TypeOperand),
    xor: binopInstr(TypeOperand),

    // comparison
    gt: binopInstr(TypeOperand),
    lt: binopInstr(TypeOperand),
    ge: binopInstr(TypeOperand),
    le: binopInstr(TypeOperand),
    eq: binopInstr(TypeOperand),
    neq: binopInstr(TypeOperand),

    // birwise
    andb: binopInstr(IntegerTypeOperand),
    orb: binopInstr(IntegerTypeOperand),
    xorb: binopInstr(IntegerTypeOperand),

    // jumps
    jmp: { operands: [] },
    jnz: { operands: [] },

    // push to stack
    push8: { operands: [{ type: "number", bytes: 1 }] },
    push16: { operands: [{ type: "number", bytes: 2 }] },
    push32: { operands: [{ type: "number", bytes: 4 }] },
    push64: { operands: [{ type: "number", bytes: 8 }] },

    // duplicate top of stack
    dup: { operands: [{ type: "number", bytes: 1 }] },

    // pop off stack
    pop: { operands: [{ type: "number", bytes: 3 }] },

    // binary and bitwise NOT
    not: { operands: [{ type: "enum", variants: TypeOperand }] },
    notb: { operands: [{ type: "enum", variants: IntegerTypeOperand }] },

    // move top of stack
    mov: {
      operands: [
        { type: "number", bytes: 4 },
        { type: "number", bytes: 3 },
      ],
    },

    // cast srctype to dsttype
    cast: {
      operands: [
        { type: "enum", variants: TypeOperand },
        { type: "enum", variants: TypeOperand },
      ],
    },
  },
  true
);

function binopInstr(types: readonly OperandVariant[]) {
  return {
    operands: [
      {
        type: "enum",
        variants: types,
      },
      {
        type: "enum",
        variants: types,
      },
    ] as const,
  } satisfies InstructionSpec<any>;
}
