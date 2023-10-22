import { ExecutionError, ParseExpr, automap, defaultExprCheck } from "../ast";
import { Operator } from "../lexing";
import { ParseNode } from "../parser-utils";
import { ExecutionContext } from "../runtime/runtime";
import {
  IParseExpr,
  TypecheckContext,
  typecheckBinaryOperation,
} from "../typecheck";

export function handleBinaryOperation(ctx: ExecutionContext, op: Operator) {
  const right = ctx.popTempValueAndGetBoth();
  const left = ctx.popTempValueAndGetBoth();

  let lv = left.value;
  let rv = right.value;

  switch (op) {
    // member access
    // TODO: do this later cuz it'll be difficult
    case "->":
    case ".":
      return ctx;
  }

  if (lv instanceof ArrayBuffer || rv instanceof ArrayBuffer) {
    throw new ExecutionError("Cannot do this operation between structs", ctx);
  }

  // get rid of bigints if either operand is floating-point
  if (
    left.typeinfo.type.definition.category === "float" ||
    right.typeinfo.type.definition.category === "float"
  ) {
    lv = Number(lv);
    rv = Number(rv);
    // otherwise convert both to bigint, cuz they're both ints and that's fine
  } else {
    lv = BigInt(lv);
    rv = BigInt(rv);
  }

  let output;

  // TODO: implement horrible C typecasting rules
  let outputType = left.typeinfo.type;

  // pointer arithmetic weirdness with + and -
  if (
    (op === "+" || op === "-") &&
    (left.typeinfo.type.pointers > 0 || right.typeinfo.type.pointers > 0)
  ) {
    rv = Number(rv);
    lv = Number(lv);

    // is the left arg the pointer
    const leftIsPtr = left.typeinfo.type.pointers > 0;

    // multply right value by -1
    if (op === "-") {
      rv *= -1;
    }

    // pointer value and non-pointer value
    const ptrVal = leftIsPtr ? lv : rv;
    const nonPtrVal = leftIsPtr ? rv : lv;

    // pointer type and non-pointer type
    const ptrType = leftIsPtr ? left.typeinfo : right.typeinfo;
    const nonPtrType = leftIsPtr ? right.typeinfo : left.typeinfo;

    // do pointer arithmetic and figure out output type
    output =
      ptrVal +
      nonPtrVal *
        ctx.sizeof({
          definition: ptrType.type.definition,
          pointers: ptrType.type.pointers - 1,
        });
    outputType = ptrType.type;

    // push to stack
    ctx.pushAnonymous(outputType, output);

    return ctx;
  }

  switch (op) {
    // arithmetic operators
    case "+":
    case "-":
    case "*":
    case "/":
    case "%":
      output = {
        "+": (a, b) => a + b,
        "-": (a, b) => a - b,
        "*": (a, b) => a * b,
        "/": (a, b) => a / b,
        "%": (a, b) => a % b,
      }[op](lv, rv);
      break;

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
      output = Number(
        {
          "==": (a, b) => a == b,
          "!=": (a, b) => a != b,
          ">=": (a, b) => a >= b,
          ">": (a, b) => a > b,
          "<=": (a, b) => a <= b,
          "<": (a, b) => a < b,
        }[op](lv, rv)
      );
      break;

    // TODO: disallow bitwise on floats
    // bitwise
    case "&":
    case "^":
    case "|":
    case "<<":
    case ">>":
      output = {
        "&": (a, b) => a & b,
        "^": (a, b) => a ^ b,
        "|": (a, b) => a | b,
        ">>": (a, b) => a >> b,
        "<<": (a, b) => a << b,
      }[op](lv, rv);
      break;
  }

  if (output !== undefined) {
    ctx.pushAnonymous(outputType, output);
  }

  return ctx;
}

export class BinaryOpNode
  extends ParseNode<{
    left: ParseExpr;
    right: ParseExpr;
    op: Operator;
  }>
  implements IParseExpr
{
  debug(): string {
    return `(${this.d.op} ${this.d.left.debug()} ${this.d.right.debug()})`;
  }

  exec(ctx: ExecutionContext) {
    ctx = ctx.clone();
    ctx = this.d.left.exec(ctx);
    ctx = this.d.right.exec(ctx);

    ctx = handleBinaryOperation(ctx, this.d.op);

    return ctx;
  }

  // TODO: Implement lvalues here later

  mapInner(cb: (node: ParseNode<any>) => void): void {
    automap(this.d.left, cb);
    automap(this.d.right, cb);
  }

  type(ctx: TypecheckContext) {
    return typecheckBinaryOperation(
      ctx,
      this.d.op,
      this.d.left,
      this.d.right,
      this
    );
  }

  *checkInner(ctx) {
    return defaultExprCheck(this, ctx);
  }
}
