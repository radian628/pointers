import { For, createEffect, createMemo } from "solid-js";
import { RunState } from "../runtime/run";
import { ExecutionContext, Type } from "../runtime/runtime";
import { ParseNode } from "../parser-utils";
import { getLineAndCol } from "../typecheck";

type VariableMemoryCorrespondence = {
  name?: string;
  creator: ParseNode<any>;
  type: Type;
  offset: number;
};

type MemoryCellMetadata = {
  variables: VariableMemoryCorrespondence[];
  value: number;
};

function getAllVariableBindings(ctx: ExecutionContext) {
  return ctx.stack
    .map((frame) =>
      frame.temporaries
        .map((t) => ({ variable: t, name: undefined }))
        .concat(
          [...frame.bindings.entries()].map((binding) => ({
            variable: binding[1],
            name: binding[0],
          }))
        )
    )
    .flat(1);
}

function generateVariableMemoryMap(ctx: ExecutionContext) {
  const allBindings = getAllVariableBindings(ctx);

  console.log(allBindings);

  const largestAddress = Math.max(
    ...allBindings.map((b) => b.variable.offset + ctx.sizeof(b.variable.type))
  );

  const metadata: MemoryCellMetadata[] = [];

  const memArray = new Uint8Array(ctx.memory);

  for (let i = 0; i < largestAddress; i++) {
    metadata.push({
      variables: [],
      value: memArray[i],
    });
  }

  for (const binding of allBindings) {
    for (let i = 0; i < ctx.sizeof(binding.variable.type); i++) {
      metadata[i + binding.variable.offset].variables.push({
        name: binding.name,
        type: binding.variable.type,
        offset: i,
        creator: binding.variable.creator,
      });
    }
  }

  return metadata;
}

export function getVarName(c: VariableMemoryCorrespondence) {
  if (c.name) return c.name;

  console.log(c);

  if (!c.creator) return;

  const start = getLineAndCol(
    c.creator.start.text(),
    c.creator.start.position()
  );
  const end = getLineAndCol(c.creator.end.text(), c.creator.end.position());

  return start.line == end.line
    ? `(${start.line}:${start.col} - ${end.col})`
    : `(${start.line}:${start.col} - ${end.line}:${end.col})`;
}

export function MemoryCell(props: { value: () => MemoryCellMetadata }) {
  return (
    <div class="memory-cell">
      <div class="memory-cell-value">
        0x{props.value().value.toString(16).padStart(2, "0")} (
        {String.fromCharCode(props.value().value)})
      </div>
      <div class="memory-cell-variables">
        <For each={props.value().variables}>
          {(v) => (
            <div>
              {getVarName(v)}+{v.offset}
            </div>
          )}
        </For>
      </div>
    </div>
  );
}

export function MemoryViewPanel(props: { output: () => ExecutionContext }) {
  const memVarMap = createMemo(() => {
    return generateVariableMemoryMap(props.output());
  });

  createEffect(() => {
    console.log("VARMAP", memVarMap());
  });

  return (
    <div class="memory-view-panel">
      <div class="memory-cell-container">
        <For each={memVarMap()}>
          {(cell) => <MemoryCell value={() => cell}></MemoryCell>}
        </For>
      </div>
    </div>
  );
}
