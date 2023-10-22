import { ExecutionError, IdentifierNode, TypeAnnotationNode } from "./ast";
import { Operator, UnaryOperator } from "./lexing";
import { ParseNode } from "./parser-utils";
import {
  DefaultPrimitives,
  FloatsBySize,
  IntsBySize,
  Type,
  TypeDefinition,
} from "./runtime/runtime";

export class TypecheckContext {
  knownTypeNames: Map<string, TypeDefinition>;
  stack: {
    blocks: { knownVariableTypes: Map<string, Type> }[];
    knownFunctionTypes: Map<
      string,
      {
        args: Type[];
        returns: Type;
      }
    >;
  }[];

  clone() {
    const tc2 = new TypecheckContext();
    tc2.knownTypeNames = new Map(this.knownTypeNames);
    tc2.stack = this.stack.slice();
  }

  getTypeFromName(node: TypeAnnotationNode): MaybeType {
    const type = this.knownTypeNames.get(node.d.name);

    if (type)
      return {
        success: true,
        type: {
          pointers: node.d.pointers,
          definition: type,
        },
      };

    return {
      success: false,
      why: [{ node, msg: `Type '${node.d.name}' does not exist.` }],
    };
  }

  getVariableType(node: IdentifierNode): MaybeType {
    const stacktop = this.stacktop();

    const name = node.d.name;

    // local vars within function
    for (const block of stacktop.blocks.slice().reverse()) {
      const v = block.knownVariableTypes.get(name);
      if (v) return { type: v, success: true };
    }

    const v = this.stack[0].blocks[0].knownVariableTypes.get(name);
    if (v) return { type: v, success: true };

    return {
      success: false,
      why: [{ node, msg: `The variable '${node.d.name}' does not exist.` }],
    };
  }

  getFunctionTypes(name: string, node: ParseNode<any>) {
    const v = this.stack[0].knownFunctionTypes.get(name);
    if (v) return { success: true as true, ...v };

    return {
      success: false as false,
      why: [{ node, msg: `The function '${name}' does not exist.` }],
    };
  }

  stacktop() {
    return this.stack[this.stack.length - 1];
  }

  blocktop() {
    return this.stacktop().blocks[this.stacktop().blocks.length - 1];
  }

  withBlock(cb: () => void) {
    this.stacktop().blocks.push({ knownVariableTypes: new Map() });
    cb();
    this.stacktop().blocks.pop();
  }

  withStackFrame(cb: () => void) {
    this.stack.push({
      blocks: [{ knownVariableTypes: new Map() }],
      knownFunctionTypes: new Map(),
    });
    cb();
    this.stack.pop();
  }

  defineVariable(name: string, type: Type) {
    this.blocktop().knownVariableTypes.set(name, type);
  }
}

export type CTypeError = {
  success: false;
  why: { node: ParseNode<any>; msg: string }[];
};

export function typeErr(node: ParseNode<any>, ...msgs: string[]): CTypeError {
  return {
    success: false,
    why: msgs.map((msg) => ({ node, msg })),
  };
}

export function typeSuccess(type: Type): { success: true; type: Type } {
  return { success: true as true, type };
}

export type MaybeType = { success: true; type: Type } | CTypeError;

export interface IParseExpr {
  type(ctx: TypecheckContext): MaybeType;
}

export function organizeTypeErrors(
  typeerrors: MaybeType[]
): [CTypeError | undefined, Type[]] {
  const errs = typeerrors.filter((t) => !t.success);
  const successes = typeerrors
    .filter((t) => t.success)
    .map((t) => (t as { success: true; type: Type }).type);

  if (errs.length === 0) return [undefined, successes];
  return [
    {
      success: false,
      why: errs
        .map((err) => {
          if (err.success) return [];
          return err.why;
        })
        .flat(1),
    },
    successes,
  ];
}

export function isStruct(type: Type) {
  return type.definition.category === "struct" && type.pointers === 0;
}

export function isPointer(type: Type) {
  return type.pointers > 0;
}

export function isFloat(type: Type) {
  return type.definition.category === "float" && type.pointers === 0;
}

export function typeToString(type: Type) {
  let basename = type.definition.name;

  if (type.definition.category === "struct") basename = "struct " + basename;

  return basename + " " + "".padStart(type.pointers, "*");
}

/*
Type checking roles for binary operations:

- Floats override ints
- Bigger overrides smaller
*/

export function combineTypesForArithmetic(
  node: ParseNode<any>,
  a: Type,
  b: Type,
  op: "+" | "-" | "*" | "/" | "%"
): MaybeType {
  const aPointer = isPointer(a);
  const bPointer = isPointer(b);

  if (aPointer && bPointer)
    return typeErr(
      node,
      "Cannot do arithmetic between a pointer and a pointer."
    );

  if (a.definition.category === "struct" || b.definition.category === "struct")
    return typeErr(node, "Cannot do arithmetic on structs.");

  if (aPointer || bPointer) {
    if (op !== "+" && op !== "-")
      return typeErr(node, `Cannot do the '${op}' operation with a pointer.`);
  }

  const biggestSize = Math.max(a.definition.size, b.definition.size) as
    | 8
    | 4
    | 2
    | 1;
  const switchToFloat =
    a.definition.category === "float" || b.definition.category === "float";
  const switchToSigned =
    a.definition.category === "int" || b.definition.category === "int";

  if (switchToFloat) {
    return typeSuccess({
      definition: FloatsBySize[biggestSize as 8 | 4],
      pointers: 0,
    });
  }

  if (switchToSigned) {
    return typeSuccess({
      definition: IntsBySize[biggestSize],
      pointers: 0,
    });
  }

  return typeSuccess(a.definition.size === biggestSize ? a : b);
}

export function typesEqual(t1: Type, t2: Type) {
  return t1.definition.name === t2.definition.name;
}

export function combineTypesForComparisonAndLogical(
  node: ParseNode<any>,
  a: Type,
  b: Type,
  op: "!=" | "==" | "<=" | "<" | ">=" | ">" | "&&" | "^^" | "||"
) {
  if ((op !== "!=" && op !== "==" && isStruct(a)) || isStruct(b))
    return typeErr(node, `Cannot use the '${op}' operator on structs.`);

  return typeSuccess({
    definition: DefaultPrimitives.char,
    pointers: 0,
  });
}

export function combineTypesForBitwise(
  node: ParseNode<any>,
  a: Type,
  b: Type,
  op: "&" | "^" | "|" | "<<" | ">>"
) {
  if (isFloat(a) || isFloat(b))
    return typeErr(
      node,
      `Cannot use the '${op}' operator on a floating-point value.`
    );
  if (isPointer(a) || isPointer(b))
    return typeErr(node, `Cannot use the '${op}' operator on pointers.`);

  // use same promotion and signed/unsigned rules as arithmetic
  return combineTypesForArithmetic(node, a, b, "+");
}

export function typecheckBinaryOperation(
  ctx: TypecheckContext,
  op: Operator,
  left: IParseExpr & ParseNode<any>,
  right: IParseExpr & ParseNode<any>,
  node: ParseNode<any>
) {
  const mltype = left.type(ctx);
  const mrtype = right.type(ctx);

  const [errs, [ltype, rtype]] = organizeTypeErrors([mltype, mrtype]);

  if (errs) return errs;

  switch (op) {
    // member access
    // TODO: do this later cuz it'll be difficult (maybe remove it due to time constraints)
    case "->":
    case ".":
      return typeErr(node, "This operation is currently not supported.");
  }

  switch (op) {
    // arithmetic operators
    case "+":
    case "-":
    case "*":
    case "/":
    case "%":
      return combineTypesForArithmetic(node, ltype, rtype, op);

    // comparison operators
    case "!=":
    case "==":
    case "<=":
    case ">":
    case ">=":
    case "<":
    case "&&":
    case "^^":
    case "||":
      return combineTypesForComparisonAndLogical(node, ltype, rtype, op);

    // TODO: disallow bitwise on floats
    // bitwise
    case "&":
    case "^":
    case "|":
    case "<<":
    case ">>":
      return combineTypesForBitwise(node, ltype, rtype, op);
  }
}

export function pointerTo(type: Type): Type {
  return {
    definition: type.definition,
    pointers: type.pointers + 1,
  };
}

export function dereference(type: Type): Type {
  return {
    definition: type.definition,
    pointers: type.pointers - 1,
  };
}

export function typecheckUnaryOperation(
  ctx: TypecheckContext,
  op: UnaryOperator,
  value: IParseExpr & ParseNode<any>,
  node: ParseNode<any>
): MaybeType {
  const mtype = value.type(ctx);

  const [errs, [type]] = organizeTypeErrors([mtype]);

  if (errs) return errs;

  if (op === "*") {
    if (!isPointer(type))
      return typeErr(
        node,
        `The '*' operator can only be applied to a pointer type, as it represents the dereferencing of a pointer.`
      );

    return typeSuccess(dereference(type));
  } else if (op === "!") {
    if (isStruct(type))
      return typeErr(
        node,
        `The '!' operator cannot be applied to a struct type.`
      );

    return typeSuccess({
      definition: DefaultPrimitives.char,
      pointers: 0,
    });
  } else if (op === "~") {
    if (isFloat(type))
      return typeErr(
        node,
        `The '~' operator cannot be applied to a floating point type.`
      );
    if (isStruct(type))
      return typeErr(
        node,
        `The '~' operator cannot be applied to a struct type.`
      );

    return typeSuccess(type);
  }

  // & operator
  const lv = value.typeLValue(ctx);
  const [errs2, [lvalueType]] = organizeTypeErrors([lv]);

  if (errs2) return errs2;

  return typeSuccess(pointerTo(lvalueType));
}
