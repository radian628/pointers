import { ParseExpr, automap } from "../ast";
import { Operator } from "../lexing";
import { ParseNode } from "../parser-utils";
import { ExecutionContext } from "../runtime/runtime";
import { organizeTypeErrors } from "../typecheck";
import { handleBinaryOperation } from "./BinaryOpNode";

export class AssignmentNode extends ParseNode<{
  left: ParseExpr;
  right: ParseExpr;
  op?: Operator;
}> {
  debug(): string {
    return `(${
      this.d.op ?? ""
    }= ${this.d.left.debug()} ${this.d.right.debug()})`;
  }

  exec(ctx: ExecutionContext) {
    ctx = ctx.clone(this);

    ctx = this.d.left.execLValue(ctx);

    if (this.d.op) {
      ctx = this.d.left.exec(ctx);
    }

    ctx = this.d.right.exec(ctx);

    if (this.d.op) {
      handleBinaryOperation(ctx, this.d.op, this);
    }

    const right = ctx.popTempValueAndGetBoth();

    // must be a pointer to the thing to be assigned to
    const left = ctx.popTempValueAndGetBoth();

    ctx.setVar(
      {
        type: right.typeinfo.type,
        offset: left.value as number,
        creator: this,
      },
      right.value
    );

    return ctx;
  }

  mapInner(cb) {
    automap(this.d.left, cb);
    automap(this.d.right, cb);
  }

  *checkInner(ctx) {
    const mltype = this.d.left.typeLValue(ctx);
    const mrtype = this.d.right.type(ctx);

    const [errs, [ltype, rtype]] = organizeTypeErrors([mltype, mrtype]);

    yield errs?.why;

    // TODO: ensure that rtype is assignable to ltype
  }
}
