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
export const opRegex = [
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
export const numberTypeRegex = /[fui]/g;
export type Operator = (typeof opRegex)[number];

export const unaryOpRegex = ["!", "*", "&", "~"] as const;
export type UnaryOperator = (typeof unaryOpRegex)[number];
