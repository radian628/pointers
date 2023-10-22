import { ParseStatement, automap, handleStatementList } from "../ast";
import { ParseNode, TypeErrorFeedback } from "../parser-utils";
import { ExecutionContext } from "../runtime/runtime";
import { TypecheckContext } from "../typecheck";

export class StatementListNode extends ParseNode<{
  body: ParseStatement[];
}> {
  debug(): string {
    return this.d.body.map((s) => s.debug()).join("\n");
  }

  exec(ctx: ExecutionContext) {
    ctx = ctx.clone();

    ctx = handleStatementList(ctx, this.d.body).ctx;

    return ctx;
  }

  mapInner(cb: (node: ParseNode<any>) => void): void {
    automap(this.d.body, cb);
  }

  *checkInner(
    ctx: TypecheckContext
  ): IterableIterator<TypeErrorFeedback | TypeErrorFeedback[] | undefined> {
    for (const stmt of this.d.body) yield stmt.check(ctx);
  }
}
