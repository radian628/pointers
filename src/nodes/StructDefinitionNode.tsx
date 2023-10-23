import { autodebug, ExecutionError, automap } from "../ast";
import { ParseNode } from "../parser-utils";
import { ExecutionContext } from "../runtime/runtime";
import { TypecheckContext } from "../typecheck";
import { DefinitionNode } from "./DefinitionNode";

export class StructDefinitionNode extends ParseNode<{
  name: string;
  fields: DefinitionNode[];
}> {
  debug(): string {
    return `(struct ${this.d.name} (${autodebug(this.d.fields)}))`;
  }

  exec(ctx: ExecutionContext) {
    ctx = ctx.clone(this);

    // TODO: cehck if types and fieldtypes exist and decide what to do

    ctx.types.set(this.d.name, {
      category: "struct",
      name: this.d.name,
      fields: this.d.fields.map((f) => {
        const fieldtype = ctx.types.get(f.d.type.d.name);

        if (!fieldtype)
          throw new ExecutionError(
            `The struct field '${f.d.name}' has type '${f.d.type}', which does not exist.`,
            ctx
          );

        return [
          f.d.name,
          {
            definition: fieldtype,
            pointers: f.d.type.d.pointers,
          },
        ];
      }),
    });

    return ctx;
  }

  mapInner(cb) {
    automap(this.d.fields, cb);
  }

  *checkInner(ctx: TypecheckContext) {
    yield { node: this, msg: "Structs are currently not supported." };
  }
}
