import { ParseExpr, autodebug, automap } from "../ast";
import { ParseNode } from "../parser-utils";
import { ExecutionContext } from "../runtime/runtime";

export class ReturnStatementNode extends ParseNode<{
  expr?: ParseExpr;
}> {
  debug(): string {
    return `(return ${autodebug(this.d.expr)})`;
  }

  exec(ctx: ExecutionContext) {
    ctx = ctx.clone(this);

    ctx = this.d.expr?.exec(ctx) ?? ctx;

    let outputValue: number | bigint | ArrayBuffer = 0;

    if (this.d.expr) {
      const output = ctx.popTempValueAndGetBoth();
      outputValue = output.value;
    }

    const frame = ctx.popStackFrame();

    ctx.pushAnonymous(frame.returnType, outputValue, this);

    return ctx;
  }

  mapInner(cb: (node: ParseNode<any>) => void): void {
    automap(this.d.expr, cb);
  }

  // TODO: statically ensure that return type matches enclosing function type

  *checkInner(ctx) {
    if (this.d.expr) yield this.d.expr.check(ctx);
  }
}
