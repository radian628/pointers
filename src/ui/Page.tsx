import "./Page.less";

import { CodeEditor } from "./CodeEditor";
import { Show, createSignal } from "solid-js";
import { RunState, parse, run } from "../runtime/run";
import { formatDiagnostic } from "../typecheck";
import { MemoryViewPanel } from "./MemoryViewPanel";
import { ExecutionContext } from "../runtime/runtime";
import { ParseNode } from "../parser-utils";

const DEFAULTCODE = `int printstr(char * str) {
    while (*str != '\\0') {
        putc(*str);

        str += 1;
    }
}

int printnum(int num) {
    int placevalue = 1;
    
    while (placevalue <= num) {
        placevalue *= 10;
    }

    placevalue /= 10;

    while (placevalue > 0) {
        putc(48 + ((num / placevalue) % 10));

        placevalue /= 10;
    }
}

printstr("Welcome to the Pointers language!\\n");
printstr("The tiny not-exactly-a-subset-of-C, \\n");
printstr("    intended to teach you how pointers work!\\n");
printstr("You can print numbers too: ");
printnum(123456);
`;
export function Page() {
  const [code, setCode] = createSignal(DEFAULTCODE);

  const [output, setOutput] = createSignal<RunState>(run(code()));

  const [exec, setExec] = createSignal<ExecutionContext>();

  const [isRunning, setIsRunning] = createSignal(false);

  const [nodeHighlights, setNodeHighlights] = createSignal<ParseNode<any>[]>(
    []
  );

  return (
    <div class="page">
      <CodeEditor
        code={code}
        setCode={setCode}
        isRunning={isRunning}
        exec={exec}
        nodeHighlights={nodeHighlights}
      ></CodeEditor>
      <div class="code-output-panel">
        <div class="run-panel">
          <div class="run-row">
            <button
              class="menu-button"
              onClick={() => {
                setIsRunning(!isRunning());
                if (isRunning()) {
                  setOutput(run(code()));
                  const o = output().finalState;
                  if (o) setExec(o);
                } else {
                  setExec();
                }
              }}
            >
              {isRunning() ? "Stop" : "Run"}
            </button>
            <span
              class="run-feedback"
              style={{
                color: output().type === "error" ? "red" : "green",
              }}
            >
              {output().type === "error" ? "Error" : "Success"}
            </span>
          </div>
          <div class="run-row">
            <Show when={exec() && output().finalState}>
              <button
                class="menu-button"
                onClick={() => {
                  const prev = exec().prev;
                  if (prev) setExec(prev);
                }}
              >
                Back
              </button>
              <button
                class="menu-button"
                onClick={() => {
                  const next = exec().next;
                  if (next) setExec(next);
                }}
              >
                Forward
              </button>
              {"  "}(
              <input
                class="exec-index"
                type="number"
                min={1}
                max={output().finalState.getindex()}
                value={exec().getindex()}
                onInput={(e) => {
                  const value = Number(e.target.value);

                  setExec(exec().seekindex(value));
                }}
              ></input>{" "}
              / {output().finalState.getindex()})
            </Show>
          </div>
        </div>
        <pre class="code-output">
          {output().type === "success"
            ? output().finalState.stdout
            : output()
                .errors.map((err) => formatDiagnostic(err))
                .join("\n")}
        </pre>
      </div>
      <Show when={exec()}>
        <MemoryViewPanel
          output={exec}
          setNodeHighlights={setNodeHighlights}
        ></MemoryViewPanel>
      </Show>
    </div>
  );
}
