import {
  ErrorNode,
  TypeAnnotationNode,
  DefinitionNode,
  StatementListNode,
  ParseStatement,
  IfNode,
  ElseNode,
  AssignmentNode,
  FunctionDefNode,
  VariableDefinitionNode,
  ParseExpr,
  NumberNode,
  FunctionCallNode,
  IdentifierNode,
  BinaryOpNode,
  LoopNode,
  StructDefinitionNode,
  ReturnStatementNode,
  UnaryOpNode,
  StringLiteralNode,
  TypecastNode,
  ExecutionError,
} from "./ast";
import {
  Operator,
  UnaryOperator,
  charLiteralRegex,
  identRegex,
  numberRegex,
  numberTypeRegex,
  opEqualsRegex,
  opRegex,
  stringLiteralRegex,
  unaryOpRegex,
} from "./lexing";
import {
  BindingPowers,
  Highlight,
  Matcher,
  MutableParseSourceWrapper,
  ParseSource,
  UnaryBindingPowers,
  requiresSemicolon,
  seek,
} from "./parser-utils";

function getBindingPowerOfNextToken(s: ParseSource): number {
  const str = s.isNext(opRegex);

  if (!str) return 0;

  return BindingPowers[str as Operator] ?? 0;
}

function enclose<T>(
  start: ParseSource,
  muts: MutableParseSourceWrapper,
  end: Matcher,
  endErr: string,
  highlightEnds: Highlight,
  callback: (muts: MutableParseSourceWrapper) => T
): Exclude<T, ErrorNode>[] | ErrorNode {
  const items: T[] = [];

  while (!muts.isNext(end)) {
    const item = callback(muts);
    if (item instanceof ErrorNode) return item;
    items.push(item);
  }

  if (!muts.expect(end, highlightEnds)) return muts.err(start, endErr);

  return items as Exclude<T, ErrorNode>[];
}

export function parseTypeAnnotation(s: ParseSource) {
  const muts = s.mut();

  const struct = !!muts.expect("struct", "keyword");

  // edge case: unsigned keyword (no time to make more complex compound types with stuff like const unfortunately)
  const unsigned = struct ? undefined : muts.expect("unsigned", "type");

  let name = muts.expect(identRegex, "type");
  if (!name) return muts.err(s, "Expected a type name.");

  if (unsigned) name = `unsigned ${name}`;

  let pointers = 0;

  while (muts.isNext("*")) {
    muts.expect("*", "operator");
    pointers++;
  }

  return new TypeAnnotationNode(s, muts.current(), {
    struct,
    name,
    pointers,
  });
}

export function parseDefinition(s: ParseSource) {
  const muts = s.mut();

  const type = muts.parse(parseTypeAnnotation, 0);

  if (type instanceof ErrorNode) return type;

  const name = muts.expect(identRegex, "identifier");

  if (!name) return muts.err(s, "Expected an identifier.");

  return new DefinitionNode(s, muts.current(), {
    type,
    name,
  });
}

export function parseStatementList(
  s: ParseSource
): StatementListNode | ErrorNode {
  const muts = s.mut();
  const body: ParseStatement[] = [];
  while (true) {
    const stmt = muts.parse(parseStatement, 0);
    if (stmt instanceof ErrorNode) break;
    body.push(stmt);

    if (requiresSemicolon(stmt) && !muts.expect(";", "semicolon"))
      return muts.err(s, "Expected ';'");
  }
  return new StatementListNode(s, muts.current(), {
    body,
  });
}

function getCondition(start: ParseSource, muts: MutableParseSourceWrapper) {
  if (!muts.expect("(", "bracket")) return muts.err(start, "Expected '('");

  const condition = muts.parse(parseExpr, 0);

  if (!muts.expect(")", "bracket")) return muts.err(start, "Expected ')'");

  return condition;
}

function parseLoop(s: ParseSource) {
  const muts = s.mut();
  return muts.match(
    [
      [
        "for",
        "keyword",
        () => {
          if (!muts.expect("(", "bracket")) return muts.err(s, "Expected '('");

          let start, iter: ParseStatement | undefined;
          let condition: ParseExpr | undefined;

          if (!muts.isNext(";")) {
            start = muts.parse(parseStatement, 0);
          }
          if (!muts.expect(";", "semicolon"))
            return muts.err(s, "Expected ';'");
          if (!muts.isNext(";")) {
            condition = muts.parse(parseExpr, 0);
          }
          if (!muts.expect(";", "semicolon"))
            return muts.err(s, "Expected ';'");
          if (!muts.isNext(")")) {
            iter = muts.parse(parseStatement, 0);
          }
          if (!muts.expect(")", "bracket")) return muts.err(s, "Expected ')'");

          const body = parseCurlyBracesDelimitedBody(s, muts);

          if (body instanceof ErrorNode) return body;

          return new LoopNode(s, muts.current(), {
            body,
            conditions: {
              type: "for",
              start,
              condition,
              iter,
            },
          });
        },
      ],
      [
        "while",
        "keyword",
        () => {
          const condition = getCondition(s, muts);

          if (condition instanceof ErrorNode) return condition;

          const body = parseCurlyBracesDelimitedBody(s, muts);

          if (body instanceof ErrorNode) return body;

          return new LoopNode(s, muts.current(), {
            conditions: {
              type: "while",
              condition,
            },
            body,
          });
        },
      ],
    ],
    () => muts.err(s, "Expected 'for' or 'while'.")
  );
}

function parseIfElseStatement(s: ParseSource) {
  let muts = s.mut();
  return muts.match(
    [
      [
        "if",
        "keyword",
        () => {
          const condition = getCondition(s, muts);

          if (condition instanceof ErrorNode) return condition;

          const body = parseCurlyBracesDelimitedBody(s, muts);

          if (body instanceof ErrorNode) return body;

          let elseif: IfNode | ElseNode | undefined;

          if (muts.isNext("else")) {
            elseif = muts.parse(parseIfElseStatement, 0);
          }

          return new IfNode(s, muts.current(), {
            condition,
            body,
            elseif,
          });
        },
      ],
      [
        "else",
        "keyword",
        () => {
          if (muts.isNext("if")) return muts.parse(parseIfElseStatement, 0);

          const body = parseCurlyBracesDelimitedBody(s, muts);

          if (body instanceof ErrorNode) return body;

          return new ElseNode(s, muts.current(), {
            body,
          });
        },
      ],
    ],
    () => muts.err(s, "Expected 'if'.")
  );
}

function parseCurlyBracesDelimitedBody(
  start: ParseSource,
  muts: MutableParseSourceWrapper
) {
  if (!muts.expect("{", "bracket")) return muts.err(start, "Expected '{'");

  return enclose(start, muts, "}", "Expected '}'", "bracket", (muts) => {
    const stmt = muts.parse(parseStatement, 0);
    if (stmt instanceof ErrorNode) return stmt;

    if (requiresSemicolon(stmt) && !muts.expect(";", "semicolon"))
      return muts.err(start, "Expected ';'");

    return stmt;
  });
}

export function parseStatement(s: ParseSource): ParseStatement {
  let muts = s.mut();

  if (muts.expect("return", "keyword")) {
    let expr: ParseExpr | undefined = muts.parse(parseExpr, 0);

    if (expr instanceof ErrorNode && muts.isNext(";")) expr = undefined;

    if (expr instanceof ErrorNode) return expr;

    return new ReturnStatementNode(s, muts.current(), {
      expr,
    });
  }

  // struct definition
  if (muts.expect("struct", "keyword")) {
    const name = muts.expect(identRegex, "identifier");

    if (!name) return muts.err(s, "Expected an identifier.");

    if (!muts.expect("{", "bracket")) return muts.err(s, "Expected '{'");

    const fields = enclose(s, muts, "}", "Expected '}'", "bracket", (muts) => {
      const def = muts.parse(parseDefinition, 0);
      if (!muts.expect(";", "semicolon")) return muts.err(s, "Expected ';'");
      return def;
    });

    if (fields instanceof ErrorNode) return fields;

    return new StructDefinitionNode(s, muts.current(), {
      fields,
      name,
    });
  }

  muts = s.mut();

  const def = muts.parse(parseDefinition, 0);

  if (def instanceof DefinitionNode && muts.isNext(";")) {
    return new VariableDefinitionNode(s, muts.current(), {
      definition: def,
    });
  }

  // doesnt start with type declaration -> probably an assignment
  if (def instanceof ErrorNode) {
    muts = s.mut();

    // TODO: properly handle variables/stuff that start with "if" and other similar cases.
    // if/else chain
    if (muts.isNext("if")) {
      const ifStmt = muts.parse(parseIfElseStatement, 0);

      return ifStmt;
    }

    // for/while loop
    if (muts.isNext("for") || muts.isNext("while")) {
      const loop = muts.parse(parseLoop, 0);

      return loop;
    }

    // variable assignment
    const assignment = (() => {
      muts = s.mut();

      const left = muts.parse(parseExpr, 0);

      if (left instanceof ErrorNode) return;

      const opEquals = muts.expect(opEqualsRegex, "operator") as
        | `${Operator | ""}=`
        | undefined;

      if (!opEquals) return;

      const op = (opEquals === "=" ? undefined : opEquals.slice(0, 1)) as
        | Operator
        | undefined;

      const right = muts.parse(parseExpr, 0);

      if (right instanceof ErrorNode) return;

      return new AssignmentNode(s, muts.current(), {
        left,
        op,
        right,
      });
    })();

    if (!assignment) return parseExpr(s);

    return assignment;
  }

  // variable or function or struct definition
  return muts.match<ParseStatement>(
    [
      [
        "(",
        "bracket",
        () => {
          const args = enclose(
            s,
            muts,
            ")",
            "Expected ')'",
            "bracket",
            (muts) => {
              const def = muts.parse(parseDefinition, 0);
              if (!muts.expect(",", "operator") && !muts.isNext(")"))
                return muts.err(s, "Expected ','");
              return def;
            }
          );

          if (args instanceof ErrorNode) return args;

          const body = parseCurlyBracesDelimitedBody(s, muts);

          if (body instanceof ErrorNode) return body;

          return new FunctionDefNode(s, muts.current(), {
            returnTypeAndName: def,
            args,
            body,
          });
        },
      ],
      [
        "=",
        "operator",
        (name) => {
          const value = muts.parse(parseExpr, 0);

          if (value instanceof ErrorNode) return value;

          return new VariableDefinitionNode(s, muts.current(), {
            value,
            definition: def,
          });
        },
      ],
      [
        ";",
        "semicolon",
        () => {
          return new VariableDefinitionNode(s, muts.current(), {
            definition: def,
          });
        },
      ],
    ],
    () => {
      return parseExpr(s);
    }
  );

  return parseExpr(s);
}

export function parseExpr(s: ParseSource): ParseExpr {
  // parse initial expr, returning it if it's an error
  let left = parseInitExpr(s);
  if (left instanceof ErrorNode) return left;
  let currentSrc = left.end;

  while (true) {
    // set binding power and leave early if applicable
    const nextBindingPower = getBindingPowerOfNextToken(currentSrc);
    if (nextBindingPower <= s.bindingPower()) break;

    // try to parse the consequent expr, leaving early on error
    const consequent = parseConsequentExpr(
      left.setBindingPower(nextBindingPower)
    );
    if (consequent instanceof ErrorNode) break;

    left = consequent;
  }

  return left;
}

function decodeString(str: string) {
  let decodedStr = "";

  for (let i = 0; i < str.length; i++) {
    if (str[i] === "\\") {
      switch (str[++i]) {
        case "t":
        case "r":
        case "n":
        case "0":
        case "\\":
          decodedStr += {
            t: "\t",
            r: "\r",
            n: "\n",
            "0": "\0",
            "\\": "\\",
          }[str[i]];
          break;
        case "x": {
          const num = parseInt(str[++i] + str[++i], 16);
          decodedStr += String.fromCharCode(num);
          break;
        }
        case "u": {
          const num = parseInt(str[++i] + str[++i] + str[++i] + str[++i], 16);
          decodedStr += String.fromCharCode(num);
          break;
        }
      }
    } else {
      decodedStr += str[i];
    }
  }

  return decodedStr;
}

function parseInitExpr(s: ParseSource): ParseExpr {
  let muts = s.mut();
  return muts.match<ParseExpr>(
    [
      // char literal
      [
        charLiteralRegex,
        "string",
        (str) => {
          const char = decodeString(str.slice(1, -1));

          return new NumberNode(s, muts.current(), {
            num: Number(char.charCodeAt(0)),
            type: "i",
            bytes: 1,
          });
        },
      ],

      // string literal
      [
        stringLiteralRegex,
        "string",
        (inputStr) => {
          const str = decodeString(inputStr.slice(1, -1));

          return new StringLiteralNode(s, muts.current(), {
            str,
            pointer: 0,
          });
        },
      ],

      // unary op
      [
        unaryOpRegex,
        "operator",
        (op) => {
          const value = muts.parse(parseExpr, UnaryBindingPowers[op]);

          return new UnaryOpNode(s, muts.current(), {
            value,
            op: op as UnaryOperator,
          });
        },
      ],
      // parenthesized
      [
        "(",
        "bracket",
        () => {
          let beforeTypecast = muts.current();

          const typecast = muts.parse(parseTypeAnnotation, 0);

          // typecast
          if (typecast instanceof TypeAnnotationNode) {
            const typecastNode = (() => {
              if (!muts.expect(")", "bracket")) return;

              const value = muts.parse(parseExpr, 150);
              if (value instanceof ErrorNode) return;

              return new TypecastNode(s, muts.current(), {
                type: typecast,
                value,
              });
            })();

            if (typecastNode) return typecastNode;
          }

          muts = beforeTypecast.mut();

          const expr = muts.parse(parseExpr, 0);

          if (!muts.expect(")", "bracket")) return muts.err(s, "Expected ')'.");

          return expr.setParserPointer(muts.current());
        },
      ],
      // number
      [
        numberRegex,
        "number",
        (num) => {
          const numtype =
            (muts.expect(numberTypeRegex, "number") as
              | "f"
              | "u"
              | "i"
              | undefined) ?? (num.includes(".") ? "f" : "i");
          return new NumberNode(s, muts.current(), {
            num: Number(num),
            type: numtype,
          });
        },
      ],

      [
        identRegex,
        "identifier",
        (ident) => {
          return muts.match<ParseExpr>(
            [
              // function call
              [
                "(",
                "bracket",
                () => {
                  let args: ParseExpr[] = [];

                  // argument list
                  while (!muts.isNext(")")) {
                    const arg = muts.parse<ParseExpr>(parseExpr, 0);
                    args.push(arg);
                    if (!muts.expect(",", "comma")) break;
                  }

                  // closing brace
                  if (!muts.expect(")", "bracket")) {
                    seek(s, muts, ")", "bracket", "Expected a ')'");
                    args.push(muts.err(s, "Malformed function argument(s)."));
                  }

                  return new FunctionCallNode(s, muts.current(), {
                    args,
                    name: ident,
                  });
                },
              ],
            ],

            // identifier
            () => new IdentifierNode(s, muts.current(), { name: ident })
          );
        },
      ],
    ],
    () => muts.err(s, "Expected a number or an identifier.")
  );
}

function parseConsequentExpr(left: ParseExpr) {
  const muts = left.end.mut();
  console.log(left.text());
  return muts.match<ParseExpr>(
    [
      // binary op
      [
        opRegex,
        "operator",
        (op) => {
          console.log("found op", op);

          const bp = getBindingPowerOfNextToken(left.end);

          const right = muts.parse(parseExpr, bp);

          return new BinaryOpNode(left.start, muts.current(), {
            left,
            right,
            op: op as Operator,
          });
        },
      ],
    ],
    () => muts.err(left.start, "Expected a binary operator.")
  );
}
