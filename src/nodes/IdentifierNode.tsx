import { ExecutionError, defaultExprCheck } from "../ast";
import { ParseNode } from "../parser-utils";
import { ExecutionContext } from "../runtime/runtime";
import { IParseExpr, TypecheckContext, MaybeType } from "../typecheck";

export class IdentifierNode
  extends ParseNode<{ name: string }>
  implements IParseExpr
{
  debug(): string {
    return this.d.name;
  }

  exec(ctx: ExecutionContext) {
    ctx = ctx.clone();

    const data = ctx.getvar(this.d.name);

    if (!data) {
      throw new ExecutionError(
        `Identifier '${this.d.name}' does not exist.`,
        ctx
      );
    }

    ctx.pushAnonymous(data.type, ctx.getVar(data));

    return ctx;
  }

  execLValue(ctx: ExecutionContext): ExecutionContext {
    ctx = ctx.clone();

    const data = ctx.getvar(this.d.name);

    if (!data) {
      throw new ExecutionError(
        `Identifier '${this.d.name}' does not exist.`,
        ctx
      );
    }

    ctx.pushAnonymous(
      {
        definition: data.type.definition,
        pointers: data.type.pointers + 1,
      },
      data.offset
    );

    return ctx;
  }

  mapInner() {}

  type(ctx: TypecheckContext): MaybeType {
    return ctx.getVariableType(this);
  }

  *checkInner(ctx) {
    return defaultExprCheck(this, ctx);
  }
}
