import { For, createEffect, createMemo, createSignal } from "solid-js";
import { RunState } from "../runtime/run";
import {
  AnonymousVariableInstance,
  ExecutionContext,
  Type,
  VariableInstance,
  sizeof,
} from "../runtime/runtime";
import { ParseNode } from "../parser-utils";
import { dereference, getLineAndCol, isPointer } from "../typecheck";

type VariableMemoryCorrespondence = {
  name?: string;
  creator: ParseNode<any>;
  type: Type;
  offset: number;
  v: AnonymousVariableInstance;
};

type MemoryCellMetadata = {
  variables: VariableMemoryCorrespondence[];
  value: number;
  i: number;
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

  const largestAddress = Math.max(
    ...allBindings.map((b) => b.variable.offset + ctx.sizeof(b.variable.type))
  );

  const metadata: MemoryCellMetadata[] = [];

  const memArray = new Uint8Array(ctx.memory);

  for (let i = 0; i < largestAddress; i++) {
    metadata.push({
      variables: [],
      value: memArray[i],
      i,
    });
  }

  for (const binding of allBindings) {
    for (let i = 0; i < ctx.sizeof(binding.variable.type); i++) {
      metadata[i + binding.variable.offset].variables.push({
        name: binding.name,
        type: binding.variable.type,
        offset: i,
        creator: binding.variable.creator,
        v: binding.variable,
      });
    }
  }

  return metadata;
}

export function getVarName(c: VariableMemoryCorrespondence) {
  if (c.name) return c.name;

  if (!c.creator) return;

  return "TEMP";
}

export function MemoryCell(props: {
  highlight: () => boolean;
  value: () => MemoryCellMetadata;
  setNodeHighlights: (hl: ParseNode<any>[]) => void;
  setHighlightCellLow: (low: number) => void;
  setHighlightCellHigh: (high: number) => void;
  exec: () => ExecutionContext;
}) {
  return (
    <div
      classList={{
        "memory-cell": true,
        "highlighted-memory-cell": props.highlight(),
      }}
      onMouseEnter={() => {
        props.setNodeHighlights(props.value().variables.map((v) => v.creator));

        // is this a pointer?
        const ptr = props.value().variables.find((v) => isPointer(v.type));

        if (ptr) {
          let ptrval = props.exec().getVar(ptr.v);
          if (typeof ptrval !== "number") ptrval = -99999;
          console.log(ptrval);
          props.setHighlightCellLow(ptrval);
          props.setHighlightCellHigh(ptrval + sizeof(dereference(ptr.type)));
        } else {
          props.setHighlightCellLow(-1);
          props.setHighlightCellHigh(-1);
        }
      }}
    >
      <div class="memory-cell-addr">{props.value().i.toString(16)}</div>
      <div class="memory-cell-contents">
        <div class="memory-cell-value">
          {props.value().value.toString(16).padStart(2, "0")}{" "}
          {String.fromCharCode(props.value().value)}
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
    </div>
  );
}

export function MemoryViewPanel(props: {
  output: () => ExecutionContext;
  setNodeHighlights: (hl: ParseNode<any>[]) => void;
}) {
  const memVarMap = () => generateVariableMemoryMap(props.output());

  const [highlightCellLow, setHighlightCellLow] = createSignal(-1);
  const [highlightCellHigh, setHighlightCellHigh] = createSignal(-1);

  return (
    <div
      class="memory-view-panel"
      onMouseLeave={() => {
        props.setNodeHighlights([]);
        setHighlightCellHigh(-1);
        setHighlightCellLow(-1);
      }}
    >
      <div class="memory-cell-container">
        <For each={memVarMap()}>
          {(cell) => (
            <MemoryCell
              exec={props.output}
              highlight={() =>
                cell.i >= highlightCellLow() && cell.i < highlightCellHigh()
              }
              setNodeHighlights={props.setNodeHighlights}
              value={() => cell}
              setHighlightCellLow={setHighlightCellLow}
              setHighlightCellHigh={setHighlightCellHigh}
            ></MemoryCell>
          )}
        </For>
      </div>
    </div>
  );
}
