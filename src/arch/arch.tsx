import { InstructionSpec, OperandSpec, OperandVariant } from "./arch-gen";

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

new AssemblyInstructionGenerator({
  add: binaryOperatorInstruction(TypeOperand)
  },
});

function binaryOperatorInstruction(
  types: OperandVariant[]
) {
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

const popMsg = "Pop two operands from the top of the stack, ";
const pushBackMsg = ", and then push the result back onto the stack.";

const fmtStackOp = (op: string) => popMsg + op + pushBackMsg;

const Instructions: InstructionSpec[] = [
  ...[
    // arithmetic
    {
      name: "add",
      op: "+",
      desc: fmtStackOp("add them together"),
    },
    {
      name: "sub",
      op: "-",
      desc: fmtStackOp("subtract one from the other"),
    },
    {
      name: "mul",
      op: "*",
      desc: fmtStackOp("multiply them together"),
    },
    {
      name: "div",
      op: "/",
      desc: fmtStackOp("divide one by the other"),
    },
    {
      name: "mod",
      op: "%",
      desc: fmtStackOp("divide one by the other, take the remainder"),
    },

    // logical
    {
      name: "and",
      op: "&&",
      desc: fmtStackOp("take the logical AND of them"),
    },
    {
      name: "or",
      op: "||",
      desc: fmtStackOp("take the logical OR of them"),
    },
    {
      name: "xor",
      op: "^^",
      desc: fmtStackOp("take the logical XOR of them"),
    },

    // comparison
    {
      name: "gt",
      op: ">",
      desc: fmtStackOp(
        "determine whether the first is greater than the second"
      ),
    },
    {
      name: "lt",
      op: "<",
      desc: fmtStackOp("determine whether the first is less than the second"),
    },
    {
      name: "ge",
      op: ">=",
      desc: fmtStackOp(
        "determine whether the first is greater than or equal to the second"
      ),
    },
    {
      name: "le",
      op: "<=",
      desc: fmtStackOp(
        "determine whether the first is less than or equal to the second"
      ),
    },
    {
      name: "eq",
      op: "==",
      desc: fmtStackOp("determine whether the first is equal to the second"),
    },
    {
      name: "neq",
      op: "!=",
      desc: fmtStackOp(
        "determine whether the first is not equal to the second"
      ),
    },
  ].map((instr) =>
    binaryOperatorInstruction(
      instr.name,
      instr.desc,
      `Given A ${instr.op} B, this operand is the type of A.`,
      `Given A ${instr.op} B, this operand is the type of B.`,
      TypeOperand
    )
  ),

  ...[
    {
      name: "andb",
      op: "&",
      desc: fmtStackOp("take the bitwise AND of them"),
    },
    {
      name: "orb",
      op: "|",
      desc: fmtStackOp("take the bitwise OR of them"),
    },
    {
      name: "xorb",
      op: "^",
      desc: fmtStackOp("take the bitwise XOR of them"),
    },
  ].map((instr) =>
    binaryOperatorInstruction(
      instr.name,
      instr.desc,
      `Given A ${instr.op} B, this operand is the type of A.`,
      `Given A ${instr.op} B, this operand is the type of B.`,
      IntegerTypeOperand
    )
  ),

  // jumps
  {
    name: "jmp",
    desc:
      "Pop value from the stack, treat it as a memory address, " +
      "and execute it as the next instruction.",
    operands: [],
  },
  {
    name: "jnz",
    desc:
      "Pop value from the stack and treat it as a memory address. " +
      "Pop the next value too. If it is nonzero, " +
      "jump to the memory address given as the first operand.",
    operands: [],
  },

  // push
  ...[8, 16, 32, 64].map((bits) => ({
    name: `push${bits}`,
    desc: `Push an ${bits}-bit value to the stack.`,
    operands: [
      {
        type: "number" as "number",
        bits,
        desc: `The ${bits}-bit number to push to the stack.`,
        name: "value",
      },
    ],
  })),

  // pop
  {
    name: `pop`,
    desc: `Pop a value from the stack.`,
    operands: [
      {
        type: "number",
        bits: 24,
        desc: "The number of bytes of data to pop from the stack.",
        name: "bytes",
      },
    ],
  },

  // unary operators
  {
    name: "not",
    desc: "Pop a value from the stack, take its logical NOT, and push it back to the stack.",
    operands: [
      {
        type: "enum",
        name: "operand",
        variants: TypeOperand,
        desc: "The type of the value that will be logically-negated.",
      },
    ],
  },
  {
    name: "notb",
    desc: "Pop a value from the stack, take its bitwise NOT, and push it back to the stack.",
    operands: [
      {
        type: "enum",
        name: "operand",
        variants: TypeOperand,
        desc: "The type of the value that will be bitwise-negated.",
      },
    ],
  },

  // move
  {
    name: "mov",
    desc:
      "Pop a value from the stack and pop a memory address from the stack." +
      " The value will be copied to the memory address.",
    operands: [
      {
        type: "number",
        name: "size",
        bits: 3,
        desc: "The size of the value to be moved.",
      },
    ],
  },
];
