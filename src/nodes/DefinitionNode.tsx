import { automap } from "../ast";
import { ParseNode } from "../parser-utils";
import { ExecutionContext } from "../runtime/runtime";
import { TypeAnnotationNode } from "./TypeAnnotationNode";

export class DefinitionNode extends ParseNode<{
  name: string;
  type: TypeAnnotationNode;
}> {
  debug(): string {
    return `(${this.d.type.debug()} ${this.d.name})`;
  }
  exec(ctx: ExecutionContext) {
    throw new Error("Type definitions should not be exec()ed.");
    return ctx;
  }

  mapInner(cb) {
    automap(this.d.type, cb);
  }

  *checkInner(ctx) {
    yield this.d.type.check(ctx);
  }
}
