import { Operator, UnaryOperator } from "./lexing";
import { AssignmentNode } from "./nodes/AssignmentNode";
import { BinaryOpNode } from "./nodes/BinaryOpNode";
import { ErrorNode } from "./nodes/ErrorNode";
import { FunctionCallNode } from "./nodes/FunctionCallNode";
import { FunctionDefNode } from "./nodes/FunctionDefNode";
import { IdentifierNode } from "./nodes/IdentifierNode";
import { IfNode } from "./nodes/IfNode";
import { LoopNode } from "./nodes/LoopNode";
import { NumberNode } from "./nodes/NumberNode";
import { ReturnStatementNode } from "./nodes/ReturnStatementNode";
import { StatementListNode } from "./nodes/StatementListNode";
import { StringLiteralNode } from "./nodes/StringLiteralNode";
import { StructDefinitionNode } from "./nodes/StructDefinitionNode";
import { TypecastNode } from "./nodes/TypecastNode";
import { UnaryOpNode } from "./nodes/UnaryOpNode";
import { VariableDefinitionNode } from "./nodes/VariableDefinitionNode";
export { AssignmentNode } from "./nodes/AssignmentNode";
export { BinaryOpNode } from "./nodes/BinaryOpNode";
export { ErrorNode } from "./nodes/ErrorNode";
export { FunctionCallNode } from "./nodes/FunctionCallNode";
export { FunctionDefNode } from "./nodes/FunctionDefNode";
export { IdentifierNode } from "./nodes/IdentifierNode";
export { IfNode } from "./nodes/IfNode";
export { LoopNode } from "./nodes/LoopNode";
export { NumberNode } from "./nodes/NumberNode";
export { ReturnStatementNode } from "./nodes/ReturnStatementNode";
export { StatementListNode } from "./nodes/StatementListNode";
export { StringLiteralNode } from "./nodes/StringLiteralNode";
export { StructDefinitionNode } from "./nodes/StructDefinitionNode";
export { TypecastNode } from "./nodes/TypecastNode";
export { UnaryOpNode } from "./nodes/UnaryOpNode";
export { VariableDefinitionNode } from "./nodes/VariableDefinitionNode";
export { DefinitionNode } from "./nodes/DefinitionNode";
export { ElseNode } from "./nodes/ElseNode";
export { TypeAnnotationNode } from "./nodes/TypeAnnotationNode";
import { ParseNode, TypeErrorFeedback } from "./parser-utils";
import {
  AnonymousVariableInstance,
  DefaultPrimitives,
  DefaultTypes,
  ExecutionContext,
  FloatsBySize,
  IntsBySize,
  StackFrame,
  UintsBySize,
  assertPrimitiveTypeDef,
  constructTypeFromNode,
  execAndRetrieveData,
} from "./runtime/runtime";
import {
  CTypeError,
  IParseExpr,
  MaybeType,
  TypecheckContext,
  combineTypesForArithmetic,
  combineTypesForBitwise,
  combineTypesForComparisonAndLogical,
  isStruct,
  organizeTypeErrors,
  typeErr,
  typeSuccess,
  typeToString,
  typecheckBinaryOperation,
  typecheckUnaryOperation,
} from "./typecheck";

export type ParseExpr =
  | ErrorNode
  | NumberNode
  | IdentifierNode
  | FunctionCallNode
  | BinaryOpNode
  | UnaryOpNode
  | StringLiteralNode
  | TypecastNode;

// TODO: deal with expression statements leaving extra garbage on the stack
//       do not misalign the stack!!!!
export type ParseStatement =
  | ParseExpr
  | AssignmentNode
  | FunctionDefNode
  | VariableDefinitionNode
  | IfNode
  | StructDefinitionNode
  | LoopNode
  | ReturnStatementNode;

export function autodebug(node?: ParseNode<any> | ParseNode<any>[]) {
  if (Array.isArray(node)) {
    return node.map((n) => n.debug()).join(" ");
  } else if (node) {
    return node.debug();
  }
  return "";
}

export function handleStatementList(
  ctx: ExecutionContext,
  body: ParseStatement[]
) {
  const top = ctx.stacktop();
  ctx.pushBlock();
  for (const stmt of body) {
    const oldesp = ctx.esp;
    ctx = stmt.exec(ctx);
    while (oldesp > ctx.esp) ctx.popTempValue();

    // if we freed the current stack frame, exit early
    if (top.freed) {
      return {
        returned: true,
        ctx,
      };
    }
  }
  ctx.popBlock();
  return {
    returned: false,
    ctx,
  };
}

export function defaultExprCheck(
  expr: IParseExpr & ParseNode<any>,
  ctx: TypecheckContext
) {
  const type = expr.type(ctx);
  if (type.success === true) return [];
  return type.why;
}

export class ExecutionError extends Error {
  ctx: ExecutionContext;

  constructor(msg: string, ctx: ExecutionContext) {
    super(msg);
    this.ctx = ctx;
  }
}

export function automap(
  node: ParseNode<any> | ParseNode<any>[] | undefined,
  callback: (n: ParseNode<any>) => void
) {
  if (Array.isArray(node)) {
    node.map((n) => n.map(callback));
  } else if (node instanceof ParseNode) {
    node.map(callback);
  }
}
