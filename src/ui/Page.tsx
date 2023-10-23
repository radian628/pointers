import "./Page.less";

import { CodeEditor } from "./CodeEditor";
import { Show, createSignal } from "solid-js";
import { RunState, parse, run } from "../runtime/run";
import { formatDiagnostic } from "../typecheck";
import { MemoryViewPanel } from "./MemoryViewPanel";
import { ExecutionContext } from "../runtime/runtime";

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

  return (
    <div class="page">
      <CodeEditor
        code={code}
        setCode={setCode}
        isRunning={isRunning}
        exec={exec}
      ></CodeEditor>
      <div class="code-output-panel">
        <div class="run-panel">
          <button
            class="run-button"
            onClick={() => {
              setIsRunning(!isRunning());
              if (isRunning()) {
                console.log(parse(code()));
                setOutput(run(code()));
                const o = output().finalState;
                console.log("FINALSTATE", o);
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
          <Show when={exec()}>
            <button
              onClick={() => {
                const prev = exec().prev;
                if (prev) setExec(prev);
              }}
            >
              Back
            </button>
            <button
              onClick={() => {
                const next = exec().next;
                if (next) setExec(next);
              }}
            >
              Forward
            </button>
          </Show>
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
        <MemoryViewPanel output={exec}></MemoryViewPanel>
      </Show>
    </div>
  );
}
