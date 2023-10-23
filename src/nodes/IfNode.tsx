import {
  ParseExpr,
  ParseStatement,
  autodebug,
  automap,
  handleStatementList,
} from "../ast";
import { ParseNode, TypeErrorFeedback } from "../parser-utils";
import { ExecutionContext } from "../runtime/runtime";
import { TypecheckContext } from "../typecheck";
import { ElseNode } from "./ElseNode";

export class IfNode extends ParseNode<{
  condition: ParseExpr;
  body: ParseStatement[];
  elseif?: IfNode | ElseNode;
}> {
  debug(): string {
    return `(if ${this.d.condition.debug()} (${autodebug(this.d.body)}) ${
      this.d.elseif ? `else ${autodebug(this.d.elseif)}` : ""
    })`;
  }

  exec(ctx: ExecutionContext) {
    ctx = ctx.clone(this);

    ctx = this.d.condition.exec(ctx);

    const top = ctx.popTempValueAndGetData();

    // top is 0 --> go to else
    if (top == 0) {
      ctx = this.d.elseif?.exec(ctx) ?? ctx;

      // top is non-0 --> execute this
    } else {
      ctx = handleStatementList(ctx, this.d.body).ctx;
    }

    return ctx;
  }

  mapInner(cb) {
    automap(this.d.condition, cb);
    automap(this.d.body, cb);
    automap(this.d.elseif, cb);
  }

  *checkInner(ctx: TypecheckContext) {
    yield this.d.condition.check(ctx);
    const checks: TypeErrorFeedback[] = [];
    ctx.withBlock(() => {
      for (const stmt of this.d.body) checks.push(...stmt.check(ctx));
    });
    yield checks;
    yield this.d.elseif?.check(ctx);
  }
}
