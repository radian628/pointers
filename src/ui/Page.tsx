import "./Page.less";

import { CodeEditor } from "./CodeEditor";
import { createSignal } from "solid-js";
import { RunState, parse, run } from "../runtime/run";
import { formatDiagnostic } from "../typecheck";
import { MemoryViewPanel } from "./MemoryViewPanel";

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

  return (
    <div class="page">
      <CodeEditor code={code} setCode={setCode}></CodeEditor>
      <div class="code-output-panel">
        <div class="run-panel">
          <button
            class="run-button"
            onClick={() => {
              console.log(parse(code()));
              setOutput(run(code()));
              console.log("OUTPUT", output());
            }}
          >
            Run
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
        <pre class="code-output">
          {output().type === "success"
            ? output().finalState.stdout
            : output()
                .errors.map((err) => formatDiagnostic(err))
                .join("\n")}
        </pre>
      </div>
      <MemoryViewPanel output={output}></MemoryViewPanel>
    </div>
  );
}
