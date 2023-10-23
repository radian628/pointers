import {
  ParseStatement,
  ParseExpr,
  autodebug,
  automap,
  handleStatementList,
} from "../ast";
import { ParseNode, TypeErrorFeedback } from "../parser-utils";
import { ExecutionContext } from "../runtime/runtime";

export class LoopNode extends ParseNode<{
  body: ParseStatement[];
  conditions:
    | {
        type: "for";
        start?: ParseStatement;
        condition?: ParseExpr;
        iter?: ParseStatement;
      }
    | {
        type: "while";
        condition: ParseExpr;
      };
}> {
  debug(): string {
    if (this.d.conditions.type === "for") {
      return `(for (${autodebug(this.d.conditions.start)} ${autodebug(
        this.d.conditions.condition
      )} ${autodebug(this.d.conditions.iter)}) ${autodebug(this.d.body)})`;
    } else {
      return `(while ${this.d.conditions.condition.debug()} ${autodebug(
        this.d.body
      )})`;
    }
  }

  // TODO: deal with stack misalignment from statements that contain expressions but don't do anything with them

  exec(ctx: ExecutionContext) {
    ctx = ctx.clone(this);

    const stacktop = ctx.stacktop();

    const end = () => {
      ctx.popBlock();
      return ctx;
    };

    // while loop
    if (this.d.conditions.type === "while") {
      ctx.pushBlock();
      while (true) {
        ctx = this.d.conditions.condition.exec(ctx);

        const cond = ctx.popTempValueAndGetBoth();

        if (cond.value == 0) {
          break;
        }

        const list = handleStatementList(ctx, this.d.body);

        ctx = list.ctx;

        if (list.returned) return ctx;
      }

      // for loop
    } else {
      ctx = this.d.conditions.start?.exec(ctx) ?? ctx;
      if (stacktop.freed) return ctx;

      while (true) {
        ctx = this.d.conditions.condition?.exec(ctx) ?? ctx;
        if (stacktop.freed) return ctx;

        const cond = this.d.conditions.condition
          ? ctx.popTempValueAndGetBoth()
          : undefined;

        if (cond && cond.value == 0) break;

        const list = handleStatementList(ctx, this.d.body);
        ctx = list.ctx;
        if (list.returned) return ctx;

        ctx = this.d.conditions.iter?.exec(ctx) ?? ctx;

        if (stacktop.freed) return ctx;
      }
    }

    return end();
  }

  mapInner(cb) {
    automap(this.d.body, cb);

    if (this.d.conditions.type === "while") {
      automap(this.d.conditions.condition, cb);
    } else {
      automap(this.d.conditions.condition, cb);
      automap(this.d.conditions.iter, cb);
      automap(this.d.conditions.start, cb);
    }
  }

  // TODO: add thing for making sure conditions are valid
  *checkInner(ctx) {
    const checks: TypeErrorFeedback[] = [];

    if (this.d.conditions.type === "while") {
      ctx.withBlock(() => {
        checks.push(...(this.d.conditions.condition?.check(ctx) ?? []));
        checks.push(...this.d.body.map((b) => b.check(ctx)).flat(1));
      });
    } else {
      // need to create a second block to store "start" statement binding
      ctx.withBlock(() => {
        // only exists as a typeguard; this cond should never trigger
        if (this.d.conditions.type === "while") return;

        checks.push(...(this.d.conditions.start?.check(ctx) ?? []));
        checks.push(...(this.d.conditions.condition?.check(ctx) ?? []));
        checks.push(...(this.d.conditions.iter?.check(ctx) ?? []));
        ctx.withBlock(() => {
          checks.push(...this.d.body.map((b) => b.check(ctx)).flat(1));
        });
      });
    }

    yield checks;
  }
}
