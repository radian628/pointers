import { defaultExprCheck } from "../ast";
import { ParseNode } from "../parser-utils";
import {
  FloatsBySize,
  UintsBySize,
  IntsBySize,
  ExecutionContext,
} from "../runtime/runtime";
import { IParseExpr, TypecheckContext, MaybeType } from "../typecheck";

export class NumberNode
  extends ParseNode<{
    num: number;
    type: "f" | "u" | "i";
    bytes?: number;
  }>
  implements IParseExpr
{
  debug() {
    return this.d.num.toString();
  }

  determineNumericType() {
    const bytes = this.d.bytes ?? (this.d.type === "f" ? 8 : 4);
    switch (this.d.type) {
      case "f":
        return {
          definition: FloatsBySize[bytes],
          pointers: 0,
        };
      case "u":
        return {
          definition: UintsBySize[bytes],
          pointers: 0,
        };
      case "i":
      default:
        return {
          definition: IntsBySize[bytes],
          pointers: 0,
        };
    }
  }

  exec(ctx: ExecutionContext) {
    const ctx2 = ctx.clone(this);

    // default to 32-bit temp values
    ctx2.pushAnonymous(this.determineNumericType(), this.d.num, this);

    return ctx2;
  }

  mapInner() {}

  type(ctx: TypecheckContext): MaybeType {
    return {
      success: true,
      type: this.determineNumericType(),
    };
  }

  *checkInner(ctx) {
    yield defaultExprCheck(this, ctx);
  }
}
