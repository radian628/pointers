module GrammarTypes where

import Parsing

data BinaryOp
  = Add
  | Sub
  | Mul
  | Div
  | Remainder
  | LogicalAnd
  | LogicalOr
  | BitwiseAnd
  | BitwiseOr
  | BitwiseXor
  | BitshiftLeft
  | BitshiftRight
  | GreaterThan
  | LessThan
  | GreaterEq
  | LesserEq
  | EqualTo
  | NotEqualTo
  | MemberAccess
  | PointerMemberAccess
  | Comma
  | Typecast
  | ArraySubscript
  deriving (Show)

data UnaryOp
  = PrefixInc
  | PostfixInc
  | PrefixDec
  | PostfixDec
  | BitwiseNot
  | LogicalNot
  | Dereference
  | AddressOf
  | SizeOf
  deriving (Show)

--- Expression Types

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

data DeclarationSpecifierFirstHalf
  = StorageClassSpecifierDSFH StorageClassSpecifier
  | TypeSpecifierDSFH TypeSpecifier
  | TypeQualifierDSFH TypeQualifier
  | FunctionSpecifierDSFH FunctionSpecifier
  | AlignmentSpecifierDSFH AlignmentSpecifier
  deriving (Show)

data CSTAssignmentExpression
  = CSTAssignmentExpression
      { assignmentLeft :: CSTExpression,
        assignmentRight :: CSTExpression,
        operator :: Maybe BinaryOp
      }
  | AssignmentConditional CSTConditionalExpression
  deriving (Show)

data CSTConditionalExpression = CSTConditionalExpression
  { predicate :: CSTExpression,
    ifTrue :: CSTExpression,
    ifFalse :: CSTExpression
  }
  deriving (Show)

data StructOrUnionType = StructType | UnionType
  deriving (Show)

data StaticAssertDeclaration
  = StaticAssertDeclaration CSTExpression [Char]
  deriving (Show)

data StructDeclaration
  = StructDeclarationStaticAssert StaticAssertDeclaration
  | StructDeclaration
      { fieldType :: [TypeSpecifierOrQualifier],
        fieldDeclarators :: Maybe [StructDeclarator]
      }
  deriving (Show)

data StructDeclarator
  = StructDeclarator (Maybe [Char]) (Maybe CSTExpression)
  deriving (Show)

data StructOrUnionSpecifier = StructOrUnionSpecifier
  { structOrUnionType :: StructOrUnionType,
    structOrUnionName :: Maybe [Char],
    structOrUnionFields :: Maybe [StructDeclaration]
  }
  deriving (Show)

data EnumSpecifier
  = EnumSpecifierDeclaration [Char]
  | EnumSpecifierDefinition (Maybe [Char]) [Enumerator]
  deriving (Show)

data StorageClassSpecifier
  = SCSTypedef
  | SCSExtern
  | SCSStatic
  | SCSThreadLocal
  | SCSAuto
  | SCSRegister
  deriving (Show)

data TypeSpecifier
  = TSVoid
  | TSChar
  | TSShort
  | TSInt
  | TSLong
  | TSFloat
  | TSDouble
  | TSSigned
  | TSUnsigned
  | TSBool
  | TSComplex
  | TSAtomic CSTTypeName
  | TSStuctOrUnion StructOrUnionSpecifier
  | TSEnumSpecifier EnumSpecifier
  | TSTypedefName [Char]
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
  = ASTypeName CSTTypeName
  | ASConstantExpression CSTExpression
  deriving (Show)

data TypeSpecifierOrQualifier
  = IsTypeSpecifier TypeSpecifier
  | IsTypeQualifier TypeQualifier
  deriving (Show)

type PointerType = [[TypeQualifier]]

data AbstractDeclarator
  = AbstractDeclarator
      ( Maybe PointerType,
        Maybe [DirectAbstractDeclaratorElement]
      )
  deriving (Show)

data InitializerList = InitializerList [(Maybe Designation, Initializer)]
  deriving (Show)

data Initializer
  = InitializerListInitializer InitializerList
  | InitializerInitializer CSTExpression
  deriving (Show)

data Designator
  = ExpressionDesignator CSTExpression
  | IdentifierDesignator [Char]
  deriving (Show)

data Designation
  = Designation [Designator]
  deriving (Show)

data DirectAbstractDeclaratorElement
  = ArrayDADE (Maybe CSTExpression) [TypeQualifier] Bool
  | AbstractDeclaratorDADE AbstractDeclarator
  | AsteriskDADE
  | ParameterListDADE
  deriving (Show)

data Declarator
  = Declarator
      ( Maybe PointerType,
        DirectDeclarator
      )
  deriving (Show)

data DirectDeclarator
  = ArrayDD [DirectDeclaratorElement]
  deriving (Show)

data DirectDeclaratorElement
  = ArrayDDE

data CSTTypeNameData = SimpleType
  { typeName :: [Char],
    levelsOfIndirection :: Integer
  }
  deriving (Show)

data CSTExpressionData
  = FloatExpr
      { floatPrecision :: FloatPrecision,
        float :: Double
      }
  | IntExpr
      { precision :: Precision,
        value :: Integer,
        unsigned :: Bool
      }
  | StringLiteralExpr
      { encodingType :: StringEncodingType,
        stringData :: [Char]
      }
  | BinaryOperatorExpr
      { op :: BinaryOp,
        left :: CSTExpression,
        right :: CSTExpression
      }
  | UnaryOperatorExpr
      { unaryOp :: UnaryOp,
        unaryOperand :: CSTExpression
      }
  | FunctionCallExpr
      { fnName :: CSTExpression,
        fnArgs :: Maybe CSTExpression
      }
  | IdentifierExpr
      { name :: [Char]
      }
  | GenericSelectionExpr
      { assignment :: CSTExpression,
        associations :: [(Maybe (CSTTypeName), CSTExpression)]
      }
  | Assignment CSTAssignmentExpression
  | InitializerListExpression
      { initializerList :: InitializerList,
        initializerListTypeName :: CSTTypeName
      }
  | ArgumentExpressionListExpr [CSTExpression]
  | SizeofExpr CSTTypeName
  | AlignofExpr CSTTypeName
  deriving (Show)

data PPRange = PPRange {}

data CSTNode a = CSTExpression
  { exprData :: a,
    start :: ParserPointer,
    end :: ParserPointer
  }
  deriving (Show)

type CSTExpression = CSTNode CSTExpressionData

type CSTTypeName = CSTNode CSTTypeNameData

data CSTDeclSpec = CSTDeclSpec (DeclarationSpecifierFirstHalf, Maybe CSTDeclSpec)
  deriving (Show)

data Enumerator = Enumerator [Char] (Maybe CSTExpression)
  deriving (Show)
