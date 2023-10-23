import { ParseExpr, automap, defaultExprCheck } from "../ast";
import { ParseNode } from "../parser-utils";
import {
  ExecutionContext,
  execAndRetrieveData,
  constructTypeFromNode,
} from "../runtime/runtime";
import { IParseExpr, TypecheckContext } from "../typecheck";
import { TypeAnnotationNode } from "./TypeAnnotationNode";

export class TypecastNode
  extends ParseNode<{
    type: TypeAnnotationNode;
    value: ParseExpr;
  }>
  implements IParseExpr
{
  debug(): string {
    return `(cast ${this.d.value.debug()} to ${this.d.type.debug()})`;
  }

  exec(ctx: ExecutionContext) {
    const data = execAndRetrieveData(ctx, this.d.value);
    ctx = data.ctx;

    const type = constructTypeFromNode(ctx, this.d.type);
    ctx.pushAnonymous(type, data.data.value, this);

    return ctx;
  }

  mapInner(cb) {
    automap(this.d.type, cb);
    automap(this.d.value, cb);
  }

  type(ctx: TypecheckContext) {
    const type = ctx.getTypeFromName(this.d.type);
    return type;
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
