{-# LANGUAGE DeriveAnyClass #-}

module GrammarTypes where

import Data.List.NonEmpty (NonEmpty)
import Language.Haskell.TH.Syntax (Lift)
import Parsing

--- TOKENIZER

data Punctuator
  = PSquareOpen
  | PSquareClosed
  | PParenOpen
  | PParenClosed
  | PCurlyOpen
  | PCurlyClosed
  | PDot
  | PArrow
  | PInc
  | PDec
  | PAmpersand
  | PStar
  | PPlus
  | PMinus
  | PTilde
  | PExclamation
  | PSlash
  | PPercent
  | PShiftLeft
  | PShiftRight
  | PLessThan
  | PGreaterThan
  | PLessEqual
  | PGreaterEqual
  | PEqual
  | PNotEqual
  | PXor
  | POr
  | PLogicalAnd
  | PLogicalOr
  | PQuestionMark
  | PColon
  | PSemicolon
  | PEllipsis
  | PAssignment
  | PTimesEquals
  | PDivEquals
  | PModEquals
  | PAddEquals
  | PSubEquals
  | PLeftShiftEquals
  | PRightShiftEquals
  | PAndEquals
  | PXorEquals
  | POrEquals
  | PComma
  | PHash
  | PDoubleHash
  deriving (Show, Eq, Lift)

--- not including the digraphs cuz they're just diff symbols

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
  | KeywordUnsigned
  | KeywordVoid
  | KeywordVolatile
  | KeywordWhile
  | KeywordAlignAs
  | KeywordAlignOf
  | KeywordAtomic
  | KeywordBool
  | KeywordComplex
  | KeywordGeneric
  | KeywordImaginary
  | KeywordNoreturn
  | KeywordStaticAssert
  | KeywordThreadLocal
  deriving (Show, Eq)

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
  deriving (Show, Eq)

data FloatPrecision
  = Float
  | Double
  | LongDouble
  deriving (Show, Eq)

data CharEncodingType
  = CharEncodeTypeL
  | CharEncodeTypeSmallU
  | CharEncodeTypeBigU
  deriving (Show, Eq)

data StringEncodingType
  = CharC
  | Utf8
  | WcharT
  | Char16T
  | Char32T
  deriving (Show, Eq)

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

type TokenNode = ParseNode Char

data StringLiteralC
  = StringLiteralC
      (TokenNode [Char])
      (Maybe (TokenNode StringEncodingType))
  deriving (Show, Eq)

data IntLiteralC
  = IntLiteralC (ParseNode Char Integer) (Maybe (ParseNode Char (Precision, Bool)))
  deriving (Show, Eq)

data UniversalCharacterNameC
  = UniversalCharacterNameOneQuadC
      (ParseNode Char Integer)
  | UniversalCharacterNameTwoQuadC
      (ParseNode Char Integer)
      (ParseNode Char Integer)
  deriving (Show, Eq)

data FloatExprC
  = FloatExprC Double FloatPrecision
  deriving (Show, Eq)

data ConstantCD
  = ConstantIntCD (TokenNode IntLiteralC)
  | ConstantFloatCD (TokenNode FloatExprC)
  deriving (Show, Eq)

data Token
  = TokenKeyword Keyword
  | TokenIdentifier [Char] --- also an enum
  | TokenConstant ConstantCD
  | TokenCharacterConstant CharEncodingType [Char]
  | TokenStringLiteral StringLiteralC
  | TokenPunctuator Punctuator
  | TokenQuotedHeaderName [Char]
  | TokenBracketedHeaderName [Char]
  | TokenSkip SkipTokenC
  deriving (Show, Eq)

type CSTNode = ParseNode (ParseNode Char Token)

type StrParser = Parser Char

type TokenParser = Parser (ParseNode Char Token)

--- naming scheme
--- All types that end with "C" correspond
--- directly to something in the C grammar.

--- All types that end with "CD" are "C-derived"
--- types that aren't 1:1 with the C grammar.

--- A.2.1 Expressions

data PrimaryExpressionC
  = PrimaryExpressionIdentifierC (CSTNode [Char])
  | PrimaryExpressionConstantC (CSTNode ConstantCD)
  | PrimaryExpressionStringLiteralC (CSTNode StringLiteralC)
  | PrimaryExpressionExpressionC (CSTNode ExpressionC)
  | PrimaryExpressionGenericSelectionC (CSTNode GenericSelectionC)
  deriving (Show)

data GenericSelectionC
  = GenericSelectionC
      (CSTNode AssignmentExpressionC)
      (CSTNode GenericAssocListC)
  deriving (Show)

data GenericAssocListC
  = GenericAssocListC (NonEmpty (CSTNode GenericAssociationC))
  deriving (Show)

data GenericAssociationC
  = GenericAssociationTypeNameC (CSTNode TypeNameC) (CSTNode AssignmentExpressionC)
  | GenericAssociationDefaultC (CSTNode AssignmentExpressionC)
  deriving (Show)

data PostfixExpressionC
  = PostfixExpressionC
      (CSTNode PrimaryExpressionC)
      [CSTNode PostfixExpressionInnerCD]
  | PostfixExpressionInnerInitializerListCD
      (CSTNode TypeNameC)
      (CSTNode InitializerListC)
      [CSTNode PostfixExpressionInnerCD]
  deriving (Show)

data PostfixExpressionUnaryOpCD
  = PostfixExpressionUnaryOpIncrementCD
  | PostfixExpressionUnaryOpDecrementCD
  deriving (Show)

data PostfixExpressionInnerCD
  = PostfixExpressionInnerArraySubscriptCD (CSTNode ExpressionC)
  | PostfixExpressionInnerFunctionCallCD (CSTNode ArgumentExpressionListC)
  | PostfixExpressionInnerDotCD (CSTNode [Char])
  | PostfixExpressionInnerArrowCD (CSTNode [Char])
  | PostfixExpressionInnerUnaryOpCD (CSTNode PostfixExpressionUnaryOpCD)
  deriving (Show)

data ArgumentExpressionListC
  = ArgumentExpressionListC [CSTNode AssignmentExpressionC]
  deriving (Show)

data UnaryExpressionC
  = UnaryExpressionGeneralC (CSTNode UnaryOp) (CSTNode CastExpressionC)
  | UnaryExpressionSizeofC (CSTNode UnaryExpressionC)
  | UnaryExpressionSizeofTypeC (CSTNode TypeNameC)
  | UnaryExpressionAlignofC (CSTNode TypeNameC)
  | UnaryExpressionPostfixC (CSTNode PostfixExpressionC)
  deriving (Show)

data BinaryOpExpressionCD
  = BinaryOpExpressionCD
      (CSTNode BinaryOpExpressionCD)
      [(CSTNode BinaryOp, CSTNode BinaryOpExpressionCD)]
  | BinaryOpExpressionCastCD (CSTNode CastExpressionC)
  | BinaryOpDeleteLaterTestCD (CSTNode IntLiteralC)
  deriving (Show)

data CastExpressionC
  = CastExpressionUnaryExpressionC (CSTNode UnaryExpressionC)
  | CastExpressionCastC (CSTNode TypeNameC) (CSTNode CastExpressionC)
  deriving (Show)

type MultiplicativeExpressionC = BinaryOpExpressionCD

type AdditiveExpressionC = BinaryOpExpressionCD

type ShiftExpressionC = BinaryOpExpressionCD

type RelationalExpressionC = BinaryOpExpressionCD

type EqualityExpressionC = BinaryOpExpressionCD

type AndExpressionC = BinaryOpExpressionCD

type ExclusiveOrExpressionC = BinaryOpExpressionCD

type InclusiveOrExpressionC = BinaryOpExpressionCD

type LogicalAndExpressionC = BinaryOpExpressionCD

type LogicalOrExpressionC = BinaryOpExpressionCD

data ConditionalExpressionC
  = ConditionalExpressionBinaryOpC (CSTNode BinaryOpExpressionCD)
  | ConditionalExpressionTernaryC (CSTNode BinaryOpExpressionCD) (CSTNode ExpressionC) (CSTNode ConditionalExpressionC)
  deriving (Show)

data AssignmentExpressionC
  = AssignmentExpressionConditionalExpressionC
      (CSTNode ConditionalExpressionC)
  | AssignmentExpressionBinaryOpExpressionC
      (CSTNode UnaryExpressionC)
      (CSTNode AssignmentOperatorC)
      (CSTNode AssignmentExpressionC)
  deriving (Show)

data AssignmentOperatorC
  = AssignmentOperatorEqualsC
  | AssignmentOperatorMulC
  | AssignmentOperatorDivC
  | AssignmentOperatorModC
  | AssignmentOperatorAddC
  | AssignmentOperatorSubC
  | AssignmentOperatorLeftShiftC
  | AssignmentOperatorRightShiftC
  | AssignmentOperatorBitwiseAndC
  | AssignmentOperatorBitwiseXorC
  | AssignmentOperatorBitwiseOrC
  deriving (Show)

--- TODO: Include other variants here
data ExpressionC
  = ExpressionC (NonEmpty (CSTNode AssignmentExpressionC))
  deriving (Show)

type ConstantExpressionC =
  ConditionalExpressionC

--- A.2.2 Declarations

data DeclarationC
  = DeclarationC (CSTNode DeclarationSpecifiersC) (Maybe (CSTNode InitDeclaratorListC))
  | DeclarationStaticAssertC (CSTNode StaticAssertDeclarationC)
  deriving (Show)

data DeclarationSpecifiersC
  = DeclarationSpecifiersC (NonEmpty (CSTNode DeclarationSpecifierCD))
  deriving (Show)

data DeclarationSpecifierCD
  = DeclarationSpecifierStorageClassSpecifierCD (CSTNode StorageClassSpecifierC)
  | DeclarationSpecifierTypeSpecifierCD (CSTNode TypeSpecifierC)
  | DeclarationSpecifierTypeQualifierCD (CSTNode TypeQualifierC)
  | DeclarationSpecifierFunctionSpecifierCD (CSTNode FunctionSpecifierC)
  | DeclarationSpecifierAlignmentSpecifierCD (CSTNode AlignmentSpecifierC)
  deriving (Show)

data InitDeclaratorListC
  = InitDeclaratorListC
      (NonEmpty (CSTNode InitDeclaratorC))
  deriving (Show)

data InitDeclaratorC
  = InitDeclaratorDeclaratorC (CSTNode DeclaratorC)
  | InitDeclaratorAssignmentC (CSTNode DeclaratorC) (CSTNode InitializerC)
  deriving (Show)

data StorageClassSpecifierC
  = StorageClassSpecifierTypeDefC
  | StorageClassSpecifierExternC
  | StorageClassSpecifierStaticC
  | StorageClassSpecifierThreadLocalC
  | StorageClassSpecifierAutoC
  | StorageClassSpecifierRegisterC
  deriving (Show)

data TypeSpecifierC
  = TypeSpecifierVoidC
  | TypeSpecifierCharC
  | TypeSpecifierShortC
  | TypeSpecifierIntC
  | TypeSpecifierLongC
  | TypeSpecifierFloatC
  | TypeSpecifierDoubleC
  | TypeSpecifierSignedC
  | TypeSpecifierUnsignedC
  | TypeSpecifierBoolC
  | TypeSpecifierComplexC
  | TypeSpecifierAtomicC (CSTNode AtomicTypeSpecifierC)
  | TypeSpecifierStructOrUnionC (CSTNode StructOrUnionSpecifierC)
  | TypeSpecifierEnumSpecifierC (CSTNode EnumSpecifierC)
  | TypeSpecifierTypedefNameC (CSTNode [Char])
  deriving (Show)

data StructOrUnionSpecifierC
  = StructOrUnionSpecifierWithDecListC
      (CSTNode StructOrUnionC)
      (Maybe (CSTNode [Char]))
      (CSTNode StructDeclarationListC)
  | StructOrUnionSpecifierC
      (CSTNode StructOrUnionC)
      (CSTNode [Char])
  deriving (Show)

data StructOrUnionC
  = StructOrUnionStructC
  | StructOrUnionUnionC
  deriving (Show)

data StructDeclarationListC
  = StructDeclarationListC
      (NonEmpty (CSTNode StructDeclarationC))
  deriving (Show)

data StructDeclarationC
  = StructDeclarationC
      (CSTNode SpecifierQualifierListC)
      (Maybe (CSTNode StructDeclaratorListC))
  | StructDeclarationStaticAssertC
      (CSTNode StaticAssertDeclarationC)
  deriving (Show)

data SpecifierQualifierListC
  = SpecifierQualifierListC
      (NonEmpty (Either (CSTNode TypeSpecifierC) (CSTNode TypeQualifierC)))
  deriving (Show)

data StructDeclaratorListC
  = StructDeclaratorListC (NonEmpty (CSTNode StructDeclaratorC))
  deriving (Show)

data StructDeclaratorC
  = StructDeclaratorC
      (CSTNode DeclaratorC)
  | StructDeclaratorWithExprC
      (Maybe (CSTNode DeclaratorC))
      (CSTNode ConstantExpressionC)
  deriving (Show)

data EnumSpecifierC
  = EnumSpecifierWithDataC (Maybe (CSTNode [Char])) (CSTNode EnumeratorListC)
  | EnumSpecifierC (CSTNode [Char])
  deriving (Show)

data EnumeratorListC
  = EnumeratorListC (NonEmpty (CSTNode EnumeratorC))
  deriving (Show)

data EnumeratorC
  = EnumeratorC (CSTNode [Char])
  | EnumeratorAssignmentC (CSTNode [Char]) (CSTNode ConstantExpressionC)
  deriving (Show)

data AtomicTypeSpecifierC
  = AtomicTypeSpecifierC (CSTNode TypeNameC)
  deriving (Show)

data TypeQualifierC
  = TypeQualifierConstC
  | TypeQualifierRestrictC
  | TypeQualifierVolatileC
  | TypeQualifierAtomicC
  deriving (Show)

data FunctionSpecifierC
  = FunctionSpecifierInlineC
  | FunctionSpecifierNoReturnC
  deriving (Show)

data AlignmentSpecifierC
  = AlignmentSpecifierC (Either (CSTNode TypeNameC) (CSTNode ConstantExpressionC))
  deriving (Show)

data DeclaratorC
  = DeclaratorC (Maybe (CSTNode PointerC)) (CSTNode DirectDeclaratorC)
  deriving (Show)

data DirectDeclaratorC
  = DirectDeclaratorC
      (Either (CSTNode [Char]) (CSTNode DeclaratorC))
      [CSTNode DirectDeclaratorSuffixCD]
  deriving (Show)

data DirectDeclaratorSuffixCD
  = DirectDeclaratorSuffixArrayC
      (Maybe (CSTNode TypeQualifierListC))
      (Maybe (CSTNode AssignmentExpressionC))
  | DirectDeclaratorSuffixStaticArrayC
      (Maybe (CSTNode TypeQualifierListC))
      (CSTNode AssignmentExpressionC)
  | DirectDeclaratorSuffixPointerArrayC
      (Maybe (CSTNode TypeQualifierListC))
  | DirectDeclaratorSuffixParamTypeListC
      (CSTNode ParameterTypeListC)
  | DirectDeclaratorSuffixIdentifierListC
      (Maybe (CSTNode IdentifierListC))
  deriving (Show)

data PointerC = PointerC (NonEmpty (CSTNode PointerElementCD))
  deriving (Show)

data PointerElementCD = PointerElementCD (Maybe (CSTNode TypeQualifierListC))
  deriving (Show)

data TypeQualifierListC
  = TypeQualifierListC (NonEmpty (CSTNode TypeQualifierC))
  deriving (Show)

data ParameterTypeListC
  = ParameterTypeListC (CSTNode ParameterListC) Bool
  deriving (Show)

data ParameterListC
  = ParameterListC (NonEmpty (CSTNode ParameterDeclarationC))
  deriving (Show)

data ParameterDeclarationC
  = ParameterDeclarationC
      (CSTNode DeclarationSpecifiersC)
      (Either (CSTNode DeclaratorC) (Maybe (CSTNode AbstractDeclaratorC)))
  deriving (Show)

data IdentifierListC = IdentifierListC (NonEmpty (CSTNode [Char]))
  deriving (Show)

data TypeNameC
  = TypeNameC (CSTNode SpecifierQualifierListC) (Maybe (CSTNode AbstractDeclaratorC))
  deriving (Show)

data AbstractDeclaratorC
  = AbstractDeclaratorPointerC (CSTNode PointerC)
  | AbstractDeclaratorDirectC (Maybe (CSTNode PointerC)) (CSTNode DirectAbstractDeclaratorC)
  deriving (Show)

data DirectAbstractDeclaratorC
  = DirectAbstractDeclaratorC
      (Maybe (CSTNode AbstractDeclaratorC))
      [CSTNode DirectAbstractDeclaratorElementCD]
  deriving (Show)

data DirectAbstractDeclaratorElementCD
  = DirectAbstractDeclaratorElementArray
      (Maybe (CSTNode TypeQualifierListC))
      (Maybe (CSTNode AssignmentExpressionC))
  | DirectAbstractDeclaratorStaticArray
      (Maybe (CSTNode TypeQualifierListC))
      (CSTNode AssignmentExpressionC)
  | DirectAbstractDeclaratorStarArray
  | DirectAbstractDeclaratorParameterList
      (Maybe (CSTNode ParameterTypeListC))
  deriving (Show)

data TypedefNameC = TypedefNameC (CSTNode [Char])
  deriving (Show)

data InitializerC
  = InitializerAssignmentC (CSTNode AssignmentExpressionC)
  | InitializerInitializerListC (CSTNode InitializerListC)
  deriving (Show)

data InitializerListC
  = InitializerListC
      ( NonEmpty
          ( (Maybe (CSTNode DesignationC)),
            (CSTNode InitializerC)
          )
      )
  deriving (Show)

data DesignationC = DesignationC (CSTNode DesignatorListC)
  deriving (Show)

data DesignatorListC
  = DesignatorListC (NonEmpty (CSTNode DesignatorC))
  deriving (Show)

data DesignatorC
  = DesignatorArrayC (CSTNode ConstantExpressionC)
  | DesignatorDotC (CSTNode [Char])
  deriving (Show)

data StaticAssertDeclarationC
  = StaticAssertDeclarationC
      (CSTNode ConstantExpressionC)
      (CSTNode StringLiteralC)
  deriving (Show)

--- A.2.3 Statements

data StatementC
  = StatementLabeledStatementC (CSTNode LabeledStatementC)
  | StatementCompoundStatementC (CSTNode CompoundStatementC)
  | StatementExpressionStatementC (CSTNode ExpressionStatementC)
  | StatementSelectionStatementC (CSTNode SelectionStatementC)
  | StatementIterationStatementC (CSTNode IterationStatementC)
  | StatementJumpStatementC (CSTNode JumpStatementC)
  deriving (Show)

data LabeledStatementC
  = LabeledStatementLabelC (CSTNode [Char]) (CSTNode StatementC)
  | LabeledStatementCaseC (CSTNode ConstantExpressionC) (CSTNode StatementC)
  | LabeledStatementDefaultC (CSTNode StatementC)
  deriving (Show)

data CompoundStatementC
  = CompoundStatementC (Maybe (CSTNode BlockItemListC))
  deriving (Show)

data BlockItemListC
  = BlockItemListC (NonEmpty (CSTNode BlockItemC))
  deriving (Show)

data BlockItemC
  = BlockItemDeclarationC (CSTNode DeclarationC)
  | BlockItemStatementC (CSTNode StatementC)
  deriving (Show)

data ExpressionStatementC = ExpressionStatementC (Maybe (CSTNode ExpressionC))
  deriving (Show)

data SelectionStatementC
  = SelectionStatementIfStatementC
      (CSTNode ExpressionC)
      (CSTNode StatementC)
  | SelectionStatementIfElseStatementC
      (CSTNode ExpressionC)
      (CSTNode StatementC)
      (CSTNode StatementC)
  | SelectionStatementSwitchStatementC
      (CSTNode ExpressionC)
      (CSTNode StatementC)
  deriving (Show)

data IterationStatementC
  = IterationStatementWhileC (CSTNode ExpressionC) (CSTNode StatementC)
  | IterationStatementDoWhileC (CSTNode ExpressionC) (CSTNode StatementC)
  | IterationStatementForC
      (Maybe (CSTNode ExpressionC))
      (Maybe (CSTNode ExpressionC))
      (Maybe (CSTNode ExpressionC))
      (CSTNode StatementC)
  | IterationStatementDecForC
      (Maybe (CSTNode DeclarationC))
      (Maybe (CSTNode ExpressionC))
      (Maybe (CSTNode ExpressionC))
      (CSTNode StatementC)
  deriving (Show)

data JumpStatementC
  = JumpStatementGotoC (CSTNode [Char])
  | JumpStatementContinueC
  | JumpStatementBreakC
  | JumpStatementReturnC (Maybe (CSTNode ExpressionC))
  deriving (Show)

--- A.2.4 External definitions

data TranslationUnitC
  = TranslationUnitC (NonEmpty (CSTNode ExternalDeclarationC))
  deriving (Show)

data ExternalDeclarationC
  = ExternalDeclarationFunctionDefinitionC (CSTNode FunctionDefinitionC)
  | ExternalDelcarationDeclarationC (CSTNode DeclarationC)
  deriving (Show)

data FunctionDefinitionC
  = FunctionDefinitionC
      (CSTNode DeclarationSpecifiersC)
      (CSTNode DeclaratorC)
      (Maybe (CSTNode DeclarationListC))
      (CSTNode CompoundStatementC)
  deriving (Show)

data DeclarationListC
  = DeclarationListC (NonEmpty (CSTNode DeclarationC))
  deriving (Show)

--- A.3 Preprocessing directives

--- TODO: Preprocessor crap

-- data PreprocessingFileC
--   = PreprocessingFileC (Maybe (CSTNode GroupC))

-- data GroupC
--   = GroupC [GroupPartC]

-- data GroupPartC
--   = IfSectionC