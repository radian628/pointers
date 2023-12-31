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
printstr("\\nYou can use the 'run' button to run the code.\\n");
printstr("After this, try using the back/forward buttons\\n");
printstr("    or the number input box to see how memory\\n");
printstr("    changes as the program continues executing.\\n");
`;

function loadFromQueryParam() {
  const param = new URLSearchParams(window.location.search).get("code");
  if (!param) return;
  return base64ToBytes(param);
}

// https://developer.mozilla.org/en-US/docs/Glossary/Base64

function base64ToBytes(base64) {
  const binString = atob(base64);
  return new TextDecoder().decode(
    Uint8Array.from(binString, (m) => m.codePointAt(0))
  );
}

function bytesToBase64(binString) {
  return btoa(binString);
}

export function Page() {
  const [code, setCode] = createSignal(loadFromQueryParam() ?? DEFAULTCODE);

  const [output, setOutput] = createSignal<RunState>(run(code()));

  const [exec, setExec] = createSignal<ExecutionContext>();

  const [isRunning, setIsRunning] = createSignal(false);

  const [nodeHighlights, setNodeHighlights] = createSignal<ParseNode<any>[]>(
    []
  );

  const [recentlyCopied, setRecentlyCopied] = createSignal(false);

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
          <div class="run-row">
            <button
              class="menu-button"
              onClick={(e) => {
                const link = `${window.location.origin}${
                  window.location.pathname
                }?code=${encodeURIComponent(bytesToBase64(code()))}`;

                window.history.pushState(undefined, "", link);

                navigator.clipboard.writeText(link);

                setRecentlyCopied(true);
                setTimeout(() => {
                  setRecentlyCopied(false);
                }, 1500);
              }}
            >
              {recentlyCopied() ? "Copied!" : "Permalink"}
            </button>
          </div>
        </div>
        <pre class="code-output">{exec() ? exec().stdout : ""}</pre>
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
