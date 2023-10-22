import { ExecutionError, defaultExprCheck } from "../ast";
import { ParseNode } from "../parser-utils";
import { ExecutionContext } from "../runtime/runtime";
import { IParseExpr, TypecheckContext, MaybeType } from "../typecheck";

export class ErrorNode
  extends ParseNode<{ msg: string }>
  implements IParseExpr
{
  debug() {
    return `(#ERROR# '${this.d.msg}')`;
  }

  exec(ctx: ExecutionContext) {
    throw new ExecutionError(`Parse Error: ${this.d.msg}`, ctx);
    return ctx;
  }

  mapInner(cb) {}

  type(ctx: TypecheckContext): MaybeType {
    return { success: false, why: [{ node: this, msg: this.d.msg }] };
  }

  *checkInner(ctx) {
    return defaultExprCheck(this, ctx);
  }
}
