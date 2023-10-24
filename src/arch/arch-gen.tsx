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
  bits: number;
};

export type OperandSpec = OperandEnum | OperandNumber;

export type MapOperandToValue<Op extends OperandSpec> = Op extends OperandEnum
  ? Op["variants"][number]["id"]
  : number;

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

export class AssemblyInstructionGenerator<
  Instructions extends Record<string, InstructionSpec<readonly OperandSpec[]>>
> {
  run: {
    [K in keyof Instructions]: (
      ...operands: MapOperandsToValue<Instructions[K]["operands"]>
    ) => AssemblyInstructionGenerator<Instructions>;
  };

  instructions: Instructions;

  constructor(instructions: Instructions) {
    this.instructions = instructions;
  }
}
