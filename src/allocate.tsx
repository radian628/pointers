import { StringLiteralNode } from "./ast";
import { ParseNode } from "./parser-utils";

type AllocateStringLiteralsCtx = {
  mem: ArrayBuffer;
};

export function concatArrayBuffers(a: ArrayBuffer, b: ArrayBuffer) {
  const concatted = new ArrayBuffer(a.byteLength + b.byteLength);
  const byteArray = new Uint8Array(concatted);
  const byteArrayA = new Uint8Array(a);
  const byteArrayB = new Uint8Array(b);
  for (let i = 0; i < byteArrayA.length; i++) {
    byteArray[i] = byteArrayA[i];
  }
  for (let i = 0; i < byteArrayB.length; i++) {
    byteArray[i + byteArrayA.length] = byteArrayB[i];
  }
  return concatted;
}

export function allocateStringLiterals(
  ast: ParseNode<any>,
  ctx: AllocateStringLiteralsCtx
) {
  function recurse(n: ParseNode<any>) {
    if (n instanceof StringLiteralNode) {
      const strbuf = new TextEncoder().encode(n.d.str + "\0");

      n.d.pointer = ctx.mem.byteLength;

      ctx.mem = concatArrayBuffers(ctx.mem, strbuf);
    }
  }

  ast.map(recurse);
}
