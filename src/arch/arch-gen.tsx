export type OperandVariant = {
  id: number;
  label: string;
  desc: string;
};

export type OperandSpec =
  | {
      name: string;
      type: "enum";
      variants: OperandVariant[];
      desc: string;
    }
  | {
      name: string;
      type: "number";
      bits: number;
      desc: string;
    };

export type InstructionSpec = {
  name: string;
  operands: OperandSpec[];
  desc: string;
};

export class AssemblyArchGenerator {
  constructor(instructions: InstructionSpec[]) {}
}
