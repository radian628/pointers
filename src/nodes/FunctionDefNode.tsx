import {
  ParseStatement,
  ExecutionError,
  automap,
  handleStatementList,
} from "../ast";
import { ParseNode, TypeErrorFeedback } from "../parser-utils";
import { ExecutionContext, StackFrame, Type } from "../runtime/runtime";
import { TypecheckContext, organizeTypeErrors } from "../typecheck";
import { DefinitionNode } from "./DefinitionNode";

export class FunctionDefNode extends ParseNode<{
  args: DefinitionNode[];
  returnTypeAndName: DefinitionNode;
  body: ParseStatement[];
}> {
  debug(): string {
    return `(fndef ${this.d.args
      .map((arg) => `(${arg.debug()})`)
      .join(" ")} ${this.d.body.map((v) => v.debug()).join(" ")})`;
  }

  exec(ctx: ExecutionContext) {
    ctx.addFunctionDefinition(this.d.returnTypeAndName.d.name, this);

    return ctx;
  }

  call(ctx: ExecutionContext) {
    ctx = ctx.clone();

    const ret = ctx.types.get(this.d.returnTypeAndName.d.type.d.name);

    if (!ret)
      throw new ExecutionError(
        `Return type '${this.d.returnTypeAndName.d.type.d.name}' does not exist.`,
        ctx
      );

    const frame: StackFrame = {
      base: ctx.esp,
      bindings: new Map(),
      temporaries: [],
      functionDefinitions: new Map(),
      freed: false,
      returnType: {
        definition: ret,
        pointers: this.d.returnTypeAndName.d.type.d.pointers,
      },
      argc: this.d.args.length,
      blocks: [
        {
          bindings: new Map(),
        },
      ],
    };

    let offset = 0;

    // assign bindings to args in reverse order
    // to so the last one is first
    for (const arg of this.d.args.slice().reverse()) {
      const type = ctx.types.get(arg.d.type.d.name);

      if (!type)
        throw new ExecutionError(
          `Error with argument '${arg.d.name}': Type '${arg.d.type.d.name}' does not exist.`,
          ctx
        );

      const fnargType = {
        definition: type,
        pointers: arg.d.type.d.pointers,
      };

      offset += ctx.sizeof(fnargType);

      frame.bindings.set(arg.d.name, {
        offset: ctx.esp - offset,
        type: fnargType,
        name: arg.d.name,
      });
    }

    ctx.stack.push(frame);

    ctx = handleStatementList(ctx, this.d.body).ctx;

    if (!frame.freed) {
      ctx = ctx.clone();
      ctx.popStackFrame();
    }

    return ctx;
  }

  mapInner(cb) {
    automap(this.d.args, cb);
    automap(this.d.returnTypeAndName, cb);
    automap(this.d.body, cb);
  }

  *checkInner(ctx: TypecheckContext) {
    // add function definition (do beforehand to allow recursion)

    const checks: TypeErrorFeedback[] = [];
    ctx.withStackFrame(() => {
      // typecheck fnargs
      for (const stmt of this.d.args) checks.push(...stmt.check(ctx));

      const argtypes: Type[] = [];

      // create bindings for fnargs
      for (const arg of this.d.args) {
        const mt = ctx.getTypeFromName(arg.d.type);

        const [errs, [t]] = organizeTypeErrors([mt]);

        if (errs) {
          checks.push(...errs.why);
          continue;
        }

        ctx.defineVariable(arg.d.name, t);

        argtypes.push(t);
      }

      const mrettype = ctx.getTypeFromName(this.d.returnTypeAndName.d.type);

      const [errs, [rettype]] = organizeTypeErrors([mrettype]);

      if (errs) {
        checks.push(...errs.why);
      } else {
        ctx.defineFunction(this.d.returnTypeAndName.d.name, rettype, argtypes);
      }
    });
    yield checks;
  }
}
