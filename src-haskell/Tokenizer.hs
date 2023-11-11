module Tokenizer where

import Parsing

data Keyword
  = KeywordAuto
  | KeywordBreak
  | KeywordCase
  | KeywordChar
  | KeywordConst
  | KeywordContinue
  | KeywordDefault
  | KeywordDo
  | KeywordDouble
  | KeywordElse
  | KeywordEnum
  | KeywordExtern
  | KeywordFloat
  | KeywordFor
  | KeywordGoto
  | KeywordIf
  | KeywordInline
  | KeywordInt
  | KeywordLong
  | KeywordRegister
  | KeywordRestrict
  | KeywordReturn
  | KeywordShort
  | KeywordSigned
  | KeywordSizeof
  | KeywordStatic
  | KeywordStruct
  | KeywordSwitch
  | KeywordTypedef
  | KeywordUnion

--- various general enum types

data BinaryOp
  = BinaryOpAdd
  | BinaryOpSub
  | BinaryOpMul
  | BinaryOpDiv
  | BinaryOpRemainder
  | BinaryOpLogicalAnd
  | BinaryOpLogicalOr
  | BinaryOpBitwiseAnd
  | BinaryOpBitwiseOr
  | BinaryOpBitwiseXor
  | BinaryOpBitshiftLeft
  | BinaryOpBitshiftRight
  | BinaryOpGreaterThan
  | BinaryOpLessThan
  | BinaryOpGreaterEq
  | BinaryOpLesserEq
  | BinaryOpEqualTo
  | BinaryOpNotEqualTo
  | BinaryOpMemberAccess
  | BinaryOpPointerMemberAccess
  | BinaryOpComma
  | BinaryOpTypecast
  | BinaryOpArraySubscript
  deriving (Show)

data UnaryOp
  = UnaryOpPrefixInc
  | UnaryOpPostfixInc
  | UnaryOpPrefixDec
  | UnaryOpPostfixDec
  | UnaryOpBitwiseNot
  | UnaryOpLogicalNot
  | UnaryOpDereference
  | UnaryOpAddressOf
  | UnaryOpPlus
  | UnaryOpMinus
  deriving (Show)

data Precision
  = Char
  | Short
  | Int
  | Long
  | LongLong
  deriving (Show)

data FloatPrecision
  = Float
  | Double
  | LongDouble
  deriving (Show)

data StringEncodingType
  = CharC
  | Utf8
  | WcharT
  | Char16T
  | Char32T
  deriving (Show)

data TypeQualifier
  = TQConst
  | TQRestrict
  | TQVolatile
  | TQAtomic
  deriving (Show)

data FunctionSpecifier
  = FSInline
  | FSNoReturn
  deriving (Show)

data AlignmentSpecifier
  = ASTypeName Placeholder
  | ASConstantExpression Placeholder
  deriving (Show)

data Placeholder = Placeholder
  deriving (Show)

--- Tokens and stuff

data StringLiteralC
  = StringLiteralC
      (CSTNode [Char])
      (Maybe (CSTNode StringEncodingType))
  deriving (Show)

data IntLiteralC
  = IntLiteralC (CSTNode Integer) (Maybe (CSTNode (Precision, Bool)))
  deriving (Show)

data UniversalCharacterNameC
  = UniversalCharacterNameOneQuadC
      (CSTNode Integer)
  | UniversalCharacterNameTwoQuadC
      (CSTNode Integer)
      (CSTNode Integer)
  deriving (Show)

data FloatExprC
  = FloatExprC Double FloatPrecision
  deriving (Show)

data Token
  = TokenKeyword Keyword
  | TokenIdentifier [Char] --- also an enum
  | TokenIntegerConstant IntLiteralC
  | TokenFloatingConstant FloatExprC
  | TokenCharacterConstant StringEncodingType Char
  | TokenStringLiteral
  | TokenPunctuator [Char]
  | TokenQuotedHeaderName [Char]
  | TokenBracketedHeaderName [Char]