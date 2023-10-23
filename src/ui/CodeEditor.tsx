import { createEffect, untrack } from "solid-js";

import { linter, Diagnostic } from "@codemirror/lint";

import { tags, Tag } from "@lezer/highlight";
import { EditorState, Extension, Range, RangeSet } from "@codemirror/state";
import {
  EditorView,
  keymap,
  Decoration,
  ViewPlugin,
  lineNumbers,
} from "@codemirror/view";
import { defaultKeymap } from "@codemirror/commands";
import {
  defaultHighlightStyle,
  syntaxHighlighting,
} from "@codemirror/language";
import { parse, run } from "../runtime/run";
import { Highlight } from "../parser-utils";

const HL2Tag: Record<Highlight, Tag> = {
  string: tags.string,
  number: tags.number,
  keyword: tags.keyword,
  type: tags.typeName,
  identifier: tags.variableName,
  bracket: tags.bracket,
  operator: tags.operator,
  semicolon: tags.operator,
  comma: tags.operator,
};

function pointerSyntaxHighlighterPlugin() {
  let decorations = RangeSet.of<Decoration>([]);

  return ViewPlugin.define(
    (view) => {
      return {
        update(update) {
          const highlights = parse(update.view.state.doc.toString()).highlights;

          decorations = RangeSet.of(
            highlights.map((hl) => {
              return Decoration.mark({
                class:
                  defaultHighlightStyle.style([HL2Tag[hl.highlight]]) ??
                  undefined,
              }).range(hl.start, hl.end);
            })
          );

          return decorations;
        },
      };
    },
    {
      decorations(update) {
        return decorations;
      },
    }
  );
}

function pointersDiagnosticPlugin() {
  return linter((view) => {
    const diagnostics: Diagnostic[] = parse(
      view.state.doc.toString()
    ).errors.map((err) => {
      return {
        message: err.msg,
        from: err.node.start.position(),
        to: err.node.end.position(),
        severity: "error",
      };
    });

    return diagnostics;
  });
}

export function CodeEditor(props: {
  code: () => string;
  setCode: (code: string) => void;
  isRunning: () => boolean;
}) {
  return (
    <div
      class="code-editor"
      ref={(el) => {
        const extensions: () => Extension = () => [
          lineNumbers(),
          syntaxHighlighting(defaultHighlightStyle, { fallback: true }),
          keymap.of(defaultKeymap),
          EditorView.updateListener.of((v) => {
            if (v.docChanged) {
              const docstring = v.state.doc.toString();
              props.setCode(docstring);
            }
          }),
          pointerSyntaxHighlighterPlugin(),
          pointersDiagnosticPlugin(),
          EditorView.editable.of(!props.isRunning()),
        ];

        const state = EditorState.create({
          doc: props.code(),
          extensions: extensions(),
        });

        createEffect(() => {
          untrack(() => {
            view.setState(
              EditorState.create({
                doc: props.code(),
                extensions: extensions(),
              })
            );
          });
        });

        const view = new EditorView({
          state,
          parent: el,
        });
      }}
    ></div>
  );
}
