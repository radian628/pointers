import { allocateStringLiterals, concatArrayBuffers } from "../allocate";
import { parseStatementList } from "../parser";
import { HighlightedRange, ParseInput } from "../parser-utils";
import {
  FunctionTypes,
  TypecheckContext,
  formatDiagnostic,
} from "../typecheck";
import { DefaultTypes, ExecutionContext, DefaultPrimitives } from "./runtime";

function retrieveNullTerminatedString(mem: ArrayBuffer, i: number) {
  const uint8array = new Uint8Array(mem);

  const dst: number[] = [];

  while (uint8array[i] != 0) {
    dst.push(uint8array[i++]);
  }

  return new Uint8Array(dst).buffer;
}

export type RunState = ReturnType<typeof run>;

const defaultFunctions: FunctionTypes = new Map([
  [
    "putc",
    {
      args: [
        {
          definition: DefaultPrimitives.int,
          pointers: 0,
        },
      ],
      returns: {
        definition: DefaultPrimitives.int,
        pointers: 0,
      },
    },
  ],
]);

export function parse(code: string) {
  const parseInput = new ParseInput(code, 0, 0);

  const tree = parseStatementList(parseInput);

  const errors = tree.check(
    new TypecheckContext(DefaultTypes, defaultFunctions)
  );

  return {
    tree,
    errors,
    highlights: tree.end.highlights(),
  };
}

export function run(code: string) {
  const parseInput = new ParseInput(code, 0, 0);

  const tree = parseStatementList(parseInput);

  const errors = tree.check(
    new TypecheckContext(DefaultTypes, defaultFunctions)
  );

  if (errors.length > 0)
    return {
      type: "error" as const,
      errors,
      highlights: tree.end.highlights(),
    };

  const globalMem = {
    mem: new ArrayBuffer(0),
  };

  allocateStringLiterals(tree, globalMem);

  const finalState = tree.exec(
    new ExecutionContext({
      littleEndian: true,
      memory: concatArrayBuffers(globalMem.mem, new ArrayBuffer(256)),
      stdout: "",
      stack: [
        {
          blocks: [
            {
              bindings: new Map(),
            },
          ],
          base: globalMem.mem.byteLength,
          bindings: new Map(),
          temporaries: [],
          functionDefinitions: new Map([
            [
              "printf",
              {
                type: "external",
                def(ctx, call) {
                  const args = call.d.args;
                  ctx = ctx.clone(call);
                  ctx = args[0].exec(ctx);
                  const value = ctx.popTempValueAndGetData();

                  const text = new TextDecoder().decode(
                    retrieveNullTerminatedString(ctx.memory, value as number)
                  );

                  ctx.stdout += text;

                  return ctx;
                },
              },
            ],
            [
              "putc",
              {
                type: "external",
                def(ctx, call) {
                  const args = call.d.args;
                  ctx = ctx.clone(call);
                  // ctx = args[0].exec(ctx);
                  const value = ctx.popTempValueAndGetData();
                  ctx.stdout += String.fromCharCode(Number(value));
                  return ctx;
                },
              },
            ],
          ]),
          freed: false,
          returnType: {
            definition: DefaultPrimitives.int,
            pointers: 0,
          },
          argc: 0,
        },
      ],
      esp: globalMem.mem.byteLength,
      types: DefaultTypes,
    })
  );

  return {
    type: "success" as const,
    finalState,
    highlights: tree.end.highlights(),
  };
}
