import { ParseExpr, automap, defaultExprCheck } from "../ast";
import { UnaryOperator } from "../lexing";
import { ParseNode } from "../parser-utils";
import { ExecutionContext, DefaultPrimitives } from "../runtime/runtime";
import {
  IParseExpr,
  MaybeType,
  TypecheckContext,
  typeErr,
  typecheckUnaryOperation,
} from "../typecheck";

export class UnaryOpNode
  extends ParseNode<{
    value: ParseExpr;
    op: UnaryOperator;
  }>
  implements IParseExpr
{
  debug(): string {
    return `(${this.d.op} ${this.d.value.debug()})`;
  }

  exec(ctx: ExecutionContext) {
    ctx = ctx.clone();
    ctx =
      this.d.op === "&" ? this.d.value.execLValue(ctx) : this.d.value.exec(ctx);

    const value = ctx.popTempValueAndGetBoth();

    let output;

    let outputType;

    // TODO: disallow float with bitwise

    switch (this.d.op) {
      case "!":
        output = value.value == 0 ? 1 : 0;
        outputType = DefaultPrimitives.bool;
        break;
      case "&":
        output = value.value;
        outputType = {
          ...value.typeinfo.type,
          pointers: value.typeinfo.type.pointers + 1,
        };
        break;
      case "*":
        outputType = {
          ...value.typeinfo.type,
          pointers: value.typeinfo.type.pointers - 1,
        };
        output = ctx.getVar({
          offset: Number(value.value),
          type: outputType,
        });
        break;
      case "~":
        output = ~value.value;
        outputType = value.typeinfo.type;
        break;
    }

    ctx.pushAnonymous(outputType, output);

    return ctx;
  }

  mapInner(cb) {
    automap(this.d.value, cb);
  }

  type(ctx: TypecheckContext) {
    return typecheckUnaryOperation(ctx, this.d.op, this.d.value, this);
  }

  *checkInner(ctx) {
    yield defaultExprCheck(this, ctx);
  }

  typeLValue(ctx: TypecheckContext): MaybeType {
    if (this.d.op !== "*") {
      return typeErr(this, `This expression cannot be an lvalue.`);
    }

    // typecheck dereference operator as normal
    return typecheckUnaryOperation(ctx, this.d.op, this.d.value, this);
  }
}
