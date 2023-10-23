const hexDigit = "[0-9a-fA-F]";

const stringLiteralWithEscapeCodesAnd = (strs: string[]) =>
  [
    "\\\\(n|t|r|0|\\\\)",
    `\\\\x${hexDigit}{2}`,
    `\\\\u${hexDigit}{4}`,
    ...strs,
  ].join("|");

export const charLiteralRegex = new RegExp(
  `'(${stringLiteralWithEscapeCodesAnd(["[^']"])})'`
);
export const stringLiteralRegex = new RegExp(
  `"(${stringLiteralWithEscapeCodesAnd(['[^"]'])})*"`
);
export const numberRegex = /[0-9]+(\.[0-9]*)?/;
export const identRegex = /[a-zA-Z_][a-zA-Z0-9_]*/;
export const skipRegex = /[ \r\t\n]+/;

// https://stackoverflow.com/questions/3561493/is-there-a-regexp-escape-function-in-javascript
function escapeRegex(string) {
  return string.replace(/[/\-\\^$*+?.()|[\]{}]/g, "\\$&");
}

export const opSymbols = [
  "+",
  "-",
  "*",
  "/",
  "||",
  "&&",
  "^^",
  "==",
  "!=",
  ">=",
  "<=",
  ">",
  "<",
  "&",
  "|",
  "^",
  "<<",
  ">>",
  ".",
  "->",
  "%",
] as const;

export const opRegex = new RegExp(
  `(${opSymbols.map((s) => escapeRegex(s)).join("|")})(?!=)`
);

export const opEqualsRegex = new RegExp(
  `(${opSymbols.map((s) => escapeRegex(s)).join("|")}|)=`
);

export const numberTypeRegex = /[fui]/g;
export type Operator = (typeof opSymbols)[number];

export const unaryOpRegex = ["!", "*", "&", "~"] as const;
export type UnaryOperator = (typeof unaryOpRegex)[number];
