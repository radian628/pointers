import { defaultExprCheck } from "../ast";
import { ParseNode } from "../parser-utils";
import { ExecutionContext, DefaultPrimitives } from "../runtime/runtime";
import { IParseExpr, TypecheckContext, typeSuccess } from "../typecheck";

export class StringLiteralNode
  extends ParseNode<{
    str: string;
    pointer: number;
  }>
  implements IParseExpr
{
  debug() {
    return `${JSON.stringify(this.d.str)}`;
  }

  exec(ctx: ExecutionContext) {
    ctx = ctx.clone();

    ctx.pushAnonymous(this._type(), this.d.pointer);

    return ctx;
  }

  mapInner(callback: (node: ParseNode<any>) => void): void {}

  _type() {
    return {
      definition: DefaultPrimitives.char,
      pointers: 1,
    };
  }

  type(ctx: TypecheckContext) {
    return typeSuccess(this._type());
  }

  *checkInner(
    ctx: TypecheckContext
  ): IterableIterator<
    | { node: ParseNode<any>; msg: string }
    | { node: ParseNode<any>; msg: string }[]
  > {
    yield defaultExprCheck(this, ctx);
  }
}
