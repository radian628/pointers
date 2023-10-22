import {
  DefinitionNode,
  ParseExpr,
  ExecutionError,
  automap,
  autodebug,
} from "../ast";
import { ParseNode, TypeErrorFeedback } from "../parser-utils";
import { ExecutionContext } from "../runtime/runtime";
import { TypecheckContext, organizeTypeErrors } from "../typecheck";

export class VariableDefinitionNode extends ParseNode<{
  definition: DefinitionNode;
  value?: ParseExpr;
}> {
  debug(): string {
    return `(let ${this.d.definition.debug()} ${autodebug(this.d.value)})`;
  }

  exec(ctx: ExecutionContext) {
    ctx = ctx.clone();

    const typeOfThisVar = ctx.types.get(this.d.definition.d.type.d.name);

    if (!typeOfThisVar)
      throw new ExecutionError(
        `The type '${this.d.definition.d.type.d.name}' does not exist.`,
        ctx
      );

    let value: number | bigint | ArrayBuffer = 0;

    // TODO: check to see whether types properly match
    if (this.d.value) {
      ctx = this.d.value.exec(ctx);
      const thingToAssign = ctx.popTempValueAndGetBoth();
      value = thingToAssign.value;
    }

    ctx.pushNamed(
      {
        definition: typeOfThisVar,
        pointers: this.d.definition.d.type.d.pointers,
      },
      value,
      this.d.definition.d.name
    );

    return ctx;
  }

  mapInner(cb) {
    automap(this.d.definition, cb);
    automap(this.d.value, cb);
  }

  *checkInner(
    ctx: TypecheckContext
  ): IterableIterator<TypeErrorFeedback | TypeErrorFeedback[] | undefined> {
    yield this.d.definition.check(ctx);
    yield this.d.value?.check(ctx);

    const [fail, [type]] = organizeTypeErrors([
      ctx.getTypeFromName(this.d.definition.d.type),
    ]);

    yield fail?.why;

    ctx.defineVariable(this.d.definition.d.name, type);
  }
}
