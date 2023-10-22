import { ParseExpr, ExecutionError, automap, defaultExprCheck } from "../ast";
import { ParseNode } from "../parser-utils";
import { ExecutionContext } from "../runtime/runtime";
import {
  IParseExpr,
  TypecheckContext,
  MaybeType,
  organizeTypeErrors,
  isStruct,
  typeToString,
  CTypeError,
} from "../typecheck";

export class FunctionCallNode
  extends ParseNode<{
    args: ParseExpr[];
    name: string;
  }>
  implements IParseExpr
{
  debug(): string {
    return `(${this.d.name} ${this.d.args
      .map((arg) => arg.debug())
      .join(" ")})`;
  }

  exec(ctx: ExecutionContext) {
    ctx = ctx.clone();
    for (const arg of this.d.args) {
      ctx = arg.exec(ctx);
    }

    const fndef = ctx.getFunctionDefinition(this.d.name);

    if (!fndef) {
      console.log(ctx);
      throw new ExecutionError(`Function '${fndef}' does not exist.`, ctx);
    }

    if (fndef.type === "internal") {
      ctx = fndef.def.call(ctx);
    } else {
      ctx = fndef.def(ctx, this.d.args);
    }

    return ctx;
  }

  mapInner(cb) {
    automap(this.d.args, cb);
  }

  type(ctx: TypecheckContext): MaybeType {
    const functionTypeSig = ctx.getFunctionTypes(this.d.name, this);

    if (functionTypeSig.success === false) return functionTypeSig;

    // correct number of args
    if (functionTypeSig.args.length !== this.d.args.length)
      return {
        success: false,
        why: [
          {
            node: this,
            msg: `The function '${this.d.name}' takes ${functionTypeSig.args.length} arguments, but you supplied ${this.d.args.length} arguments.`,
          },
        ],
      };

    const maybeArgTypes: MaybeType[] = this.d.args.map((arg) => arg.type(ctx));

    const [errs, argTypes] = organizeTypeErrors(maybeArgTypes);

    if (errs) return errs;

    const badargs: string[] = [];

    console.log(
      "fnname",
      this.d.name,
      "argtypes",
      argTypes,
      "typesig",
      functionTypeSig.args
    );

    for (let i = 0; i < functionTypeSig.args.length; i++) {
      // only strictly match if both are structs
      if (!isStruct(argTypes[i]) && !isStruct(functionTypeSig.args[i]))
        continue;

      if (argTypes[i].definition !== functionTypeSig[i].definition)
        badargs.push(
          `Argument ${i + 1} should be of type '${typeToString(
            argTypes[i]
          )}', but you put '${typeToString(functionTypeSig[i])}'.`
        );
    }

    return { type: functionTypeSig.returns, success: true };
  }

  *checkInner(ctx) {
    yield defaultExprCheck(this, ctx);
  }
}
