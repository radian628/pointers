import { ParseNode, TypeErrorFeedback } from "../parser-utils";
import { ExecutionContext } from "../runtime/runtime";
import { TypecheckContext } from "../typecheck";

export class TypeAnnotationNode extends ParseNode<{
  struct: boolean;
  name: string;
  pointers: number;
}> {
  debug(): string {
    return `${this.d.struct ? "struct " : ""}${this.d.name} ${"".padStart(
      this.d.pointers,
      "*"
    )}`;
  }

  exec(ctx: ExecutionContext) {
    throw new Error("Type annotations should not be exec()ed.");
    return ctx;
  }

  mapInner() {}

  *checkInner(
    ctx: TypecheckContext
  ): IterableIterator<TypeErrorFeedback | TypeErrorFeedback[] | undefined> {}
}
