import {
  ParseStatement,
  autodebug,
  automap,
  handleStatementList,
} from "../ast";
import { ParseNode, TypeErrorFeedback } from "../parser-utils";
import { ExecutionContext } from "../runtime/runtime";
import { TypecheckContext } from "../typecheck";

export class ElseNode extends ParseNode<{
  body: ParseStatement[];
}> {
  debug(): string {
    return `(${autodebug(this.d.body)})`;
  }

  exec(ctx: ExecutionContext) {
    ctx = ctx.clone(this);

    ctx = handleStatementList(ctx, this.d.body).ctx;

    return ctx;
  }

  mapInner(cb) {
    automap(this.d.body, cb);
  }

  *checkInner(ctx: TypecheckContext) {
    const checks: TypeErrorFeedback[] = [];
    ctx.withBlock(() => {
      for (const stmt of this.d.body) checks.push(...stmt.check(ctx));
    });
    yield checks;
  }
}
