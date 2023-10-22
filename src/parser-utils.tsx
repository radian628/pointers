import {
  ErrorNode,
  ParseExpr,
  NumberNode,
  IdentifierNode,
  FunctionCallNode,
  BinaryOpNode,
  StatementListNode,
  ParseStatement,
  IfNode,
  FunctionDefNode,
  StructDefinitionNode,
  LoopNode,
  ExecutionError,
  TypeAnnotationNode,
} from "./ast";
import { Operator, UnaryOperator, skipRegex } from "./lexing";
import { ExecutionContext, Type, TypeDefinition } from "./runtime/runtime";
import { CTypeError, MaybeType, TypecheckContext, typeErr } from "./typecheck";

export type Highlight =
  | "bracket"
  | "operator"
  | "identifier"
  | "number"
  | "comma"
  | "semicolon"
  | "keyword"
  | "type"
  | "string";

export type HighlightedRange = {
  start: number;
  end: number;
  highlight: Highlight;
  lastRange: HighlightedRange | undefined;
};

export type Matcher = string | readonly (string | RegExp)[] | RegExp;

export interface ParseSource {
  isNext(str: Matcher): string | undefined;
  expect(str: Matcher, highlight: Highlight): [string | undefined, ParseSource];
  match<T>(
    branches: [
      Matcher,
      Highlight,
      (str: string, s: ParseSource) => [T, ParseSource]
    ][],
    fallback: (s: ParseSource) => [T, ParseSource]
  ): [T, ParseSource];
  err(start: ParseSource, msg: string): [ErrorNode, ParseSource];
  bindingPower(): number;
  setBindingPower(bp: number): ParseSource;
  mut(): MutableParseSourceWrapper;
  position(): number;
  text(): string;
  highlights(): HighlightedRange[];
}

function matchOnString(matcher: Matcher, str: string): string | undefined {
  if (typeof matcher === "string") {
    return str.startsWith(matcher) ? matcher : undefined;
  } else if (Array.isArray(matcher)) {
    for (const matchStr of matcher) {
      const match = matchOnString(matchStr as string | RegExp, str);
      if (match) return match;
    }
    return undefined;
  } else if (matcher instanceof RegExp) {
    const match = matcher.exec(str);
    if (!match) return undefined;
    if (match.index === 0) return match[0];
  }
}

export class ParseInput implements ParseSource {
  src: string;
  pos: number;
  bp: number;
  // syntax highlights are stored in a linked list to avoid backtracking problems
  // while avoiding excess memory use
  _highlights?: HighlightedRange;

  highlights() {
    let hl: HighlightedRange | undefined = this._highlights;
    let highlights: HighlightedRange[] = [];
    while (hl) {
      highlights.push(hl);
      hl = hl.lastRange;
    }
    return highlights.reverse();
  }

  text() {
    return this.src;
  }

  position() {
    return this.pos;
  }

  slice() {
    return this.src.slice(this.pos);
  }

  constructor(
    src: string,
    position: number,
    bindingPower: number,
    highlights?: HighlightedRange
  ) {
    this.src = src;
    this.bp = bindingPower;
    this.pos = position;
    this._highlights = highlights;
  }

  isNext(matcher: Matcher) {
    const skipmatch = matchOnString(skipRegex, this.slice());
    const strmatch = matchOnString(
      matcher,
      this.slice().slice(skipmatch?.length ?? 0)
    );
    if (!strmatch) return undefined;
    return strmatch;
  }

  expect(
    matcher: Matcher,
    highlight: Highlight
  ): [string | undefined, ParseSource] {
    const skipmatch = matchOnString(skipRegex, this.slice());
    const strmatch = matchOnString(
      matcher,
      this.slice().slice(skipmatch?.length ?? 0)
    );
    if (!strmatch) return [undefined, this];
    const len = strmatch.length + (skipmatch?.length ?? 0);
    this._highlights = {
      highlight,
      start: this.pos,
      end: this.pos + len,
      lastRange: this._highlights,
    };
    return [
      strmatch,
      new ParseInput(
        this.src,
        this.pos + len,
        this.bindingPower(),
        this._highlights
      ),
    ];
  }

  match<T>(
    branches: [
      Matcher,
      Highlight,
      (str: string, s: ParseSource) => [T, ParseSource]
    ][],
    fallback: (s: ParseSource) => [T, ParseSource]
  ): [T, ParseSource] {
    for (const b of branches) {
      const result = this.expect(b[0], b[1]);
      if (!result[0]) continue;
      return b[2](result[0], result[1]);
    }
    return fallback(this);
  }

  err(start: ParseSource, msg: string): [ErrorNode, ParseSource] {
    return [new ErrorNode(start, this, { msg }), this];
  }

  bindingPower(): number {
    return this.bp;
  }

  setBindingPower(bp: number): ParseSource {
    return new ParseInput(this.src, this.pos, bp, this._highlights);
  }

  mut(): MutableParseSourceWrapper {
    return new MutableParseInput(this);
  }
}

export class MutableParseInput implements MutableParseSourceWrapper {
  src: ParseSource;

  constructor(src: ParseSource) {
    this.src = src;
  }

  highlights() {
    return this.src.highlights();
  }

  text() {
    return this.src.text();
  }

  position() {
    return this.src.position();
  }

  isNext(matcher: Matcher) {
    return this.src.isNext(matcher);
  }

  expect(matcher: Matcher, highlight: Highlight) {
    const [result, src] = this.src.expect(matcher, highlight);
    this.src = src;
    return result;
  }

  match<T>(
    branches: [Matcher, Highlight, (str: string) => T][],
    fallback: () => T
  ) {
    const [result, src] = this.src.match<T>(
      branches.map(
        (b) =>
          [
            b[0],
            b[1],
            (str: string, src: ParseSource) => {
              this.src = src;
              return [b[2](str), this.src];
            },
          ] as [
            Matcher,
            Highlight,
            (str: string, src: ParseSource) => [T, ParseSource]
          ]
      ),
      (src: ParseSource) => {
        this.src = src;
        return [fallback(), this.src];
      }
    );
    this.src = src;
    return result;
  }

  err(start: ParseSource, msg: string): ErrorNode {
    const [result, src] = this.src.err(start, msg);
    this.src = src;
    return result;
  }

  bindingPower() {
    return this.src.bindingPower();
  }

  setBindingPower(bp: number) {
    this.src = this.src.setBindingPower(bp);
  }

  current() {
    return this.src;
  }

  parse<PN extends ParseNode<any>>(
    nodetype: (s: ParseSource) => PN,
    bindingPower: number
  ): PN {
    const node = nodetype(this.src.setBindingPower(bindingPower));
    this.src = node.end;
    return node;
  }
}

export type MutableParseSourceWrapper = {
  [Key in Exclude<keyof ParseSource, "match" | "setBindingPower" | "mut">]: (
    ...args: Parameters<ParseSource[Key]>
  ) => ReturnType<ParseSource[Key]> extends [infer RetVal, ParseSource]
    ? RetVal
    : ReturnType<ParseSource[Key]>;
} & {
  match<T>(
    branches: [Matcher, Highlight, (str: string) => T][],
    fallback: () => T
  ): T;
  current(): ParseSource;
  parse<PN extends ParseNode<any>>(
    nodetype: (s: ParseSource) => PN,
    bindingPower: number
  ): PN;
  setBindingPower(bp: number): void;
};

export type TypeErrorFeedback = { node: ParseNode<any>; msg: string };

export abstract class ParseNode<T> {
  d: T;
  start: ParseSource;
  end: ParseSource;

  constructor(start: ParseSource, end: ParseSource, d: T) {
    this.d = d;
    this.start = start;
    this.end = end;
  }

  abstract debug(): string;

  abstract exec(ctx: ExecutionContext): ExecutionContext;

  abstract checkInner(
    ctx: TypecheckContext
  ): IterableIterator<TypeErrorFeedback | TypeErrorFeedback[] | undefined>;

  check(ctx: TypecheckContext): TypeErrorFeedback[] {
    const errors = [...this.checkInner(ctx)].flat();
    return errors.filter((e) => e) as TypeErrorFeedback[];
  }

  abstract mapInner(callback: (node: ParseNode<any>) => void): void;

  map(callback: (node: ParseNode<any>) => void) {
    callback(this);

    this.mapInner(callback);
  }

  // lvalues are treated as pointers to whatever they're being assigned to
  execLValue(ctx: ExecutionContext): ExecutionContext {
    throw new ExecutionError(
      `This expression cannot be used as an lvalue.`,
      ctx
    );
  }

  setBindingPower(bp: number) {
    // @ts-expect-error
    return new this.constructor(
      this.start,
      this.end.setBindingPower(bp),
      this.d
    );
  }

  setParserPointer(pp: ParseSource) {
    this.end = pp;
    return this;
  }

  typeLValue(ctx: TypecheckContext): MaybeType {
    return typeErr(
      this,
      "This cannot be used as an lvalue (thing that can be assigned to)."
    );
  }

  checkLValue(ctx: TypecheckContext): TypeErrorFeedback[] {
    const result = this.typeLValue(ctx);
    if (result.success) return [];
    return (result as CTypeError).why;
  }

  text() {
    return this.start.text().slice(this.start.position(), this.end.position());
  }
}

function isExpr(stmt: any): stmt is ParseExpr {
  return (
    stmt instanceof ErrorNode ||
    stmt instanceof NumberNode ||
    stmt instanceof IdentifierNode ||
    stmt instanceof FunctionCallNode ||
    stmt instanceof BinaryOpNode ||
    stmt instanceof StatementListNode
  );
}

export function requiresSemicolon(stmt: ParseStatement) {
  return !(
    stmt instanceof IfNode ||
    stmt instanceof FunctionDefNode ||
    stmt instanceof StructDefinitionNode ||
    stmt instanceof LoopNode
  );
}

export const BindingPowers: { [Key in Operator]: number } = {
  // logical
  "||": 40,
  "^^": 50,
  "&&": 60,

  // bitwise
  "|": 70,
  "^": 80,
  "&": 90,

  // comparison
  "==": 100,
  "!=": 100,
  ">=": 110,
  "<=": 110,
  ">": 110,
  "<": 110,

  // bitshift
  ">>": 120,
  "<<": 120,

  // arithmetic
  "+": 130,
  "-": 130,
  "*": 140,
  "/": 140,
  "%": 140,

  // member access
  ".": 160,
  "->": 160,
};

export const UnaryBindingPowers: { [key in UnaryOperator]: number } = {
  // unaries
  "*": 150,
  "&": 150,
  "~": 150,
  "!": 150,
};
