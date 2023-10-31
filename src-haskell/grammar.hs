import Data.Char (isDigit, isAlpha, ord, chr)

import Parsing 
import Data.Text.Lazy.Builder.Int (hexadecimal)
import Data.Foldable (toList)
import Data.Maybe (isJust, fromMaybe)
import GHC.Real ((%))
import Parsing (getpp)
import Text.Parsec (parserBind)

data BinaryOp =
  Add | Sub | Mul | Div | Remainder 
  | LogicalAnd | LogicalOr 
  | BitwiseAnd | BitwiseOr | BitwiseXor | BitshiftLeft | BitshiftRight
  | GreaterThan | LessThan | GreaterEq | LesserEq | EqualTo | NotEqualTo
  | MemberAccess | PointerMemberAccess
  | Comma
  | Typecast 
  | ArraySubscript
  deriving Show

data UnaryOp = 
  PrefixInc | PostfixInc | PrefixDec | PostfixDec
  | BitwiseNot
  | LogicalNot
  | Dereference
  | AddressOf
  | SizeOf

--- Expression Types

data Precision =
  Char | Short | Int | Long | LongLong deriving Show

data FloatPrecision = 
  Float | Double | LongDouble deriving Show

data StringEncodingType =
  CharC | Utf8 | WcharT | Char16T | Char32T deriving Show

data CSTTypeAnnotation = TODOPlaceholder
  deriving Show

data CSTAssignmentExpression = CSTAssignmentExpression {
    assignmentLeft :: CSTExpression,
    assignmentRight :: CSTExpression,
    operator :: Maybe BinaryOp
  }
  deriving Show

data CSTExpressionData = 
  FloatExpr {
    floatPrecision :: FloatPrecision,
    float :: Double
  }
  | IntExpr {
    precision :: Precision,
    value :: Integer,
    unsigned :: Bool
  } 
  | StringLiteralExpr {
    encodingType :: StringEncodingType,
    stringData :: [Char]
  } 
  | BinaryOperatorExpr {
    op :: BinaryOp,
    left :: CSTExpression,
    right :: CSTExpression
  }
  | FunctionCallExpr {
    fnName :: CSTExpression,
    fnArgs :: Maybe CSTExpression
  }
  | IdentifierExpr {
    name :: [Char]
  }
  | GenericSelectionExpr {
    assignment :: CSTAssignmentExpression,
    associations :: [(CSTTypeAnnotation, CSTAssignmentExpression)]
  }
  | Assignment CSTAssignmentExpression
  deriving Show

data CSTExpression = CSTExpression {
    exprData :: CSTExpressionData,
    start :: ParserPointer,
    end :: ParserPointer
  }
  deriving Show

wrapWithCSTExpression :: Parser CSTExpressionData -> Parser CSTExpression
wrapWithCSTExpression parser = do
  start <- getpp
  exprData <- parser
  end <- getpp
  pure CSTExpression {
    exprData,
    start,
    end
  }

-- TODO: Probably find a better way of doing this
wrapWithMaybeCSTExpression parser = do
  start <- getpp
  exprDataMaybe <- parser
  end <- getpp
  case exprDataMaybe of
    Just expr -> pure (Just CSTExpression {
        exprData = expr,
        start,
        end
      })
    Nothing -> pure Nothing

--- C grammar lifted directly from https://open-std.org/jtc1/sc22/wg14/www/docs/n1570.pdf

--- A.1.2 Keywords

keyword = paltv (map pstr [
    "auto",
    "break",
    "case",
    "char",
    "const",
    "continue",
    "default",
    "do",
    "double",
    "else",
    "enum",
    "extern",
    "float",
    "for",
    "goto",
    "if",
    "inline",
    "int",
    "long",
    "register",
    "restrict",
    "return",
    "short",
    "signed",
    "sizeof",
    "static",
    "struct",
    "switch",
    "typedef",
    "union",
    "unsigned",
    "void",
    "volatile",
    "while",
    "_Alignas",
    "_Alignof",
    "_Atomic",
    "_Bool",
    "_Complex",
    "_Generic",
    "_Imaginary",
    "_Noreturn",
    "_Static_assert",
    "_Thread_local"
  ])

--- A.1.3 Identifiers

identifierC = do
  init <- identifierNondigitC
  rest <- pkleene (paltv [digitC, identifierNondigitC])
  pure (init:rest)

identifierNondigitC :: Parser Char
identifierNondigitC = paltv [
    nonDigitC,
    universalCharacterNameC
  ]

inBetween x min max = x >= min && x <= max 

nonDigitC = pfn (\c -> inBetween (ord c) 65 90 || inBetween (ord c) 97 122 || c == '_')

digitC = pfn isDigit

--- A.1.4 Universal Character Name

universalCharacterNameC = undefined

hexQuadC = undefined

--- A.1.5 Constants

constantC = paltv [
    integerConstantC
  ]
  
integerConstantC :: Parser CSTExpressionData
integerConstantC = do 
  constant <- paltv [
      hexadecimalConstantC,
      decimalConstantC,
      octalConstantC
    ]
  suffix <- fmap (fromMaybe (Int, True)) (popt integerSuffixC)
  pure IntExpr {
      precision = fst suffix,
      value = constant,
      unsigned = snd suffix
    }

decimalConstantC :: Parser Integer
decimalConstantC = 
  fmap read (pconcatv [fmap (:[]) nonzeroDigitC, pkleene digitC] (++))

octalConstantC :: Parser Integer
octalConstantC = 
  fmap read (pconcatv [pstr "0", pkleene octalDigitC] (++))

hexadecimalConstantC :: Parser Integer
hexadecimalConstantC = 
  fmap read (pconcatv [hexadecimalPrefixC, pkleene hexadecimalDigitC] (++))

hexadecimalPrefixC = paltv [pstr "0x", pstr "0X"]

nonzeroDigitC = pfn (\c -> inBetween (ord c) 48 57)

octalDigitC = pfn (\c -> inBetween (ord c) 48 55 )

hexadecimalDigitC =
  pfn (\c -> inBetween (ord c) 48 57 || inBetween (ord c) 97 102 || inBetween (ord c) 65 70)

integerSuffixC :: Parser (Precision, Bool)
integerSuffixC = paltv [
    do
      precisionSuffix <- paltv [longLongSuffixC, longSuffixC]
      signSuffix <- popt unsignedSuffixC
      pure (precisionSuffix, isJust signSuffix),
    do
      signSuffix <- unsignedSuffixC
      precisionSuffix <- popt (paltv [longLongSuffixC, longSuffixC])
      pure (fromMaybe Int precisionSuffix, True)
  ]

unsignedSuffixC = do
  paltv [pstr "u", pstr "U"]
  pure ()

longSuffixC = do
  paltv [pstr "l", pstr "L"]
  pure Long

longLongSuffixC = do
  paltv [pstr "ll", pstr "LL"]
  pure LongLong

floatingConstant = 
  paltv [
    decimalFloatingConstantC,
    hexadecimalFloatingConstantC
  ]

decimalFloatingConstantC = do
  dbl <- fmap (read :: [Char] -> Double) (paltv [
      pconcatv [
        fractionalConstantC, poptChar exponentPartC
      ] (++),
      pconcatv [
        digitSequenceC, fmap (:[]) exponentPartC
      ] (++)
    ])
  suffix <- popt floatingSuffixC
  pure FloatExpr {
      float = dbl,
      floatPrecision = fromMaybe Double suffix
    }

hexadecimalFloatingConstantC :: Parser CSTExpressionData
hexadecimalFloatingConstantC =
  paltv [
    do
      hexadecimalPrefixC
      mantissa <- hexadecimalFractionalConstantC
      exponent <- binaryExponentPartC
      suffix <- popt floatingSuffixC
      pure FloatExpr {
          float = mantissa * (2.0 ** exponent),
          floatPrecision = fromMaybe Double suffix
        }
      ,
    do
      hexadecimalPrefixC
      (mantissa :: Double) <- fmap (read . (++) "0x") hexadecimalDigitSequenceC
      exponent <- binaryExponentPartC
      suffix <- popt floatingSuffixC
      pure FloatExpr {
          float = mantissa * (2.0 ** exponent),
          floatPrecision = fromMaybe Double suffix
        }
  ]

fractionalConstantC :: Parser [Char]
fractionalConstantC = paltv [
    pconcatv [fmap (concat . toList) (popt digitSequenceC), pstr ".", digitSequenceC] (++),
    pconcatv [digitSequenceC, pstr "."] (++)
  ]

exponentPartC = paltv [pchar 'e', pchar 'E']

signC = paltv [pchar '+', pchar '-']

digitSequenceC = pconcatv [fmap (: []) digitC, pkleene digitC] (++)

hexadecimalFractionalConstantC = paltv [
    do
      (value :: Double) <- fmap ((read . (++) "0x") . fromMaybe "0") (popt hexadecimalDigitSequenceC)
      pstr "."
      (fractional :: Double) <- fmap (read . (++) "0x") hexadecimalDigitSequenceC
      pure (value + fractional
        * (1/16) ** fromIntegral (1 + floor (logBase 16.0 fractional) :: Integer)
        ),
    do
      (value :: Double) <- fmap (read . (++) "0x") hexadecimalDigitSequenceC
      pstr "."
      pure value
  ]

stripLeadingPlus (x:xs) = if x == '+' then xs else x:xs

binaryExponentPartC :: Parser Double
binaryExponentPartC = do
  palt (pchar 'p') (pchar 'P')
  fmap (read . stripLeadingPlus) (pconcatv [poptChar signC, digitSequenceC] (++))

hexadecimalDigitSequenceC =
  pconcatv [fmap (: []) hexadecimalDigitC, pkleene hexadecimalDigitC] (++)

floatingSuffixC :: Parser FloatPrecision
floatingSuffixC =
  paltv [
    fmap (const Float) (paltv (map (fmap (: []) . pchar) "fF")),
    fmap (const Float) (paltv (map (fmap (: []) . pchar) "lL"))
  ]

enumerationConstantC = identifierC

characterConstantC = do
  prefix <- paltv (map pchar "LuU")
  pchar '\''
  seq <- cCharSequenceC
  pchar '\''
  pure (seq, prefix)

cCharSequenceC = pkleene cCharC

cCharC =
  pfn (\c -> c /= '\'' && c /= '\\' && c /= '\n')

escapeSequenceC = paltv [
    universalCharacterNameC,
    simpleEscapeSequenceC,
    octalEscapeSequenceC,
    hexadecimalEscapeSequenceC
  ]

simpleEscapeSequenceC = do
  pchar '\\'
  paltv [
      '\'' <$ pchar '\'',
      '\"' <$ pchar '\"',
      '?' <$ pchar '?',
      '\\' <$ pchar '\\',
      '\a' <$ pchar 'a',
      '\b' <$ pchar 'b',
      '\f' <$ pchar 'f',
      '\n' <$ pchar 'n',
      '\r' <$ pchar 'r',
      '\t' <$ pchar 't',
      '\v' <$ pchar 'v'
    ]

octalEscapeSequenceC :: Parser Char
octalEscapeSequenceC = do
  pchar '\\'
  fmap (chr . read . (++) "0o") (prange octalDigitC 1 3)


hexadecimalEscapeSequenceC = do
  pstr "\\x"
  fmap (chr . read . (++) "0x") (pkleene hexadecimalDigitC)

--- A.1.6 String Literals

stringLiteralC = do
  prefix <- fmap (fromMaybe CharC) (popt encodingPrefixC)
  pchar '"'
  chars <- sCharSequenceC
  pchar '"'
  pure StringLiteralExpr {
    encodingType = prefix,
    stringData = chars
  }

encodingPrefixC = paltv [
    Utf8 <$ pstr "u8",
    Char16T <$ pstr "u",
    Char32T <$ pstr "U",
    WcharT <$ pstr "L"
  ]

sCharSequenceC = pkleene sCharC

sCharC = pfn (\c -> c /= '"' && c /= '\\' && c /= '\n')

--- A.1.7 Punctuators

punctuatorC = paltv (map pstr [
    "[",
    "]",
    "{",
    "}",
    "(",
    ")",
    ".",
    "->",
    "++",
    "--",
    "&",
    "*",
    "+",
    "-",
    "~",
    "!",
    "/",
    "%",
    "<<",
    ">>",
    "<",
    ">",
    "<=",
    ">=",
    "==",
    "!=",
    "^",
    "|",
    "&&",
    "||",
    "?",
    ":",
    ";",
    "...",
    "=",
    "*=",
    "/=",
    "%=",
    "+=",
    "-=",
    "<<=",
    ">>=",
    "&=",
    "^=",
    "|=",
    ",",
    "#",
    "##",
    "<:",
    ":>",
    "<%",
    "%>",
    "%:",
    "%:%:"
  ])

--- A.1.8 Header Names

headerNameC = paltv [
    do
      pchar '<'
      chars <- hCharSequenceC
      pchar '>'
      pure chars,
    do
      pchar '"'
      chars <- qCharSequenceC
      pchar '"'
      pure chars
  ]

hCharSequenceC = pkleene qCharC

hCharC = pfn (\c -> c /= '\n' && c /= '>')

qCharSequenceC = pkleene qCharC

qCharC = pfn (\c -> c /= '\n' && c /= '"')

--- A.1.9 Preprocessing Numbers

ppNumberC = do
  --- start with optional dot
  popt (pchar '.')
  --- digits
  digits <- pkleene digitC
  --- letters and signs and stuff idk
  lettersAndSigns <- pkleene (paltv [
      do
        pchar 'e'
        signC,
      do
        pchar 'E'
        signC,
      do
        pchar 'p'
        signC,
      do
        pchar 'P'
        signC,
      identifierNondigitC,
      pchar '.'
    ])
  pure (digits, lettersAndSigns)

--- A.2 Phrase structure grammar
--- A.2.1 Expressions

primaryExpressionC :: Parser CSTExpressionData
primaryExpressionC = paltv [
    fmap (\name -> IdentifierExpr { name }) identifierC,
    constantC,
    stringLiteralC,
    do
      pchar '('
      expr <- expressionC
      pchar ')'
      pure expr,
    genericSelectionC
  ]

genericSelectionC = do
  pstr "_Generic"
  pchar '('
  assignmentExpressionC
  pchar ','
  genericAssocListC
  pchar ')'

genericAssocListC = do
  genericAssociationC
  pkleene (pconcat (pchar ',') genericAssociationC) 

genericAssociationC :: Parser CSTExpressionData
genericAssociationC = paltv [
    do
      typeNameC
      pchar ':'
      assignmentExpressionC,
    do
      pstr "default"
      pchar ':'
      assignmentExpressionC
  ]

postfixExpressionC :: Parser CSTExpressionData
postfixExpressionC = paltv [
    primaryExpressionC,
    do
      left <- wrapWithCSTExpression postfixExpressionC
      pchar '['
      right <- wrapWithCSTExpression expressionC
      pchar ']'
      pure BinaryOperatorExpr {
        op = ArraySubscript,
        left,
        right
      },
    do
      left <- wrapWithCSTExpression postfixExpressionC
      paltv [
        do
          pchar '('
          right <- wrapWithCSTExpression >>= (popt argumentExpressionListC)
          pchar ')'
          pure FunctionCallExpr {
            fnName = left,
            fnArgs = right
          },
        do    
          pchar '.'
          identifierC,
        do    
          pstr "->"
          identifierC
        ],
      pstr "++",
      pstr "--",
    do
      pchar '('
      typeNameC
      pchar ')'
      pchar '{'
      initializerListC
      pchar ','
      pchar '}'
  ]

argumentExpressionListC :: Parser CSTExpression
argumentExpressionListC = do
  argumentExpressionC
  pkleene (pconcat (pchar ',') assignmentExpressionC) 

unaryExpressionC = paltv [
    postfixExpressionC,
    do
      paltv [pstr "++", pstr "--"]
      unaryExpressionC,
    do
      unaryOperatorC
      castExpressionC,
    do
      pstr "sizeof"
      paltv [
        unaryExpressionC,
        do
          pchar '('
          typeNameC
          pchar ')'
        ],
    do
      pstr "_Alignof"
      pchar '('
      typeNameC
      pchar ')'

  ]

unaryOperatorC = paltv (map pchar "&*+-~!")

castExpressionC = paltv [
    unaryExpressionC,
    do
      pchar '('
      typeNameC
      pchar ')'
      castExpressionC
  ]

binaryOpHelper :: [[Char]] -> Parser a -> Parser a -> Parser a
binaryOpHelper operators thisParser nextParser = paltv [
    nextParser,
    do
      thisParser 
      paltv (map pstr operators)
      nextParser
  ]

multiplicativeExpressionC =
  binaryOpHelper ["*", "/", "%"] multiplicativeExpressionC castExpressionC

additiveExpressionC =
  binaryOpHelper ["+", "-"] additiveExpressionC multiplicativeExpressionC

shiftExpressionC =
  binaryOpHelper ["<<", ">>"] shiftExpressionC additiveExpressionC

relationalExpressionC =
  binaryOpHelper ["<", ">", "<=", ">="] relationalExpressionC shiftExpressionC

equalityExpressionC =
  binaryOpHelper ["==", "!="] equalityExpressionC relationalExpressionC

andExpressionC =
  binaryOpHelper ["&"] andExpressionC equalityExpressionC

exclusiveOrExpressionC = 
  binaryOpHelper ["^"] exclusiveOrExpressionC andExpressionC

inclusiveOrExpressionC =
  binaryOpHelper ["|"] inclusiveOrExpressionC exclusiveOrExpressionC

logicalAndExpressionC =
  binaryOpHelper ["&&"] logicalAndExpressionC inclusiveOrExpressionC

logicalOrExpressionC =
  binaryOpHelper ["||"] logicalOrExpressionC logicalAndExpressionC

conditionalExpressionC = paltv [
    logicalOrExpressionC,
    do
      logicalOrExpressionC
      pchar '?'
      expressionC
      pchar ':'
      conditionalExpressionC
  ]

assignmentExpressionC = paltv [
    conditionalExpressionC,
    do
      unaryExpressionC
      assignmentOperatorC
      assignmentExpressionC
  ]

assignmentOperatorC = paltv [
    pstr "=",
    pstr "*=",
    pstr "/=",
    pstr "%=",
    pstr "+=",
    pstr "-=",
    pstr "<<=",
    pstr ">>=",
    pstr "&=",
    pstr "^=",
    pstr "|="
  ]

expressionC = paltv [
    assignmentExpressionC,
    do
      expressionC
      pchar ','
      assignmentExpressionC
  ]

constantExpressionC = conditionalExpressionC

-- A.2.2 Declarations

-- declarationC

-- declarationSpecifiersC

-- initDeclaratorListC

-- initDeclaratorC

-- storageClassSpecifierC

typeSpecifierC =
  (paltv (map pstr [
    "void",
    "char",
    "short",
    "int",
    "long",
    "float",
    "double",
    "signed",
    "unsigned",
    "_Bool",
    "_Complex"
  ] ++ [
    atomicTypeSpecifierC,
    structOrUnionSpecifierC,
    enumSpecifierC,
    typedefNameC
  ]))


structOrUnionSpecifierC = paltv [
    do
      structOrUnionC
      popt identifierC
      pchar '{' 
      structDeclarationListC
      pchar '}',
    do
      structOrUnionC
      identifierC
  ]

structOrUnionC = paltv [
    pstr "struct",
    pstr "union"
  ]

structDeclarationListC = pkleene structDeclarationC

structDeclarationC = paltv [
    do
      specifierQualifierListC
      popt structDeclaratorListC
      pchar ';',
    staticAssertDeclarationC
  ]

specifierQualifierListC = paltv [
    do
      typeSpecifierC
      popt specifierQualifierListC,
    do
      typeQualifierC,
      popt specifierQualifierListC
  ]

structDeclaratorListC = do
  structDeclaratorC
  pkleene (do
    pchar ','
    structDeclaratorC)

structDeclaratorC = paltv [
    do
      popt declaratorC
      constant,
    declaratorC
  ]

-- enumSpecifierC

-- enumeratorListC

-- enumeratorC

-- atomicTypeSpecifierC

typeQualifierC = paltv (map pstr [
    "const",
    "restrict",
    "volatile",
    "_Atomic"
  ]) 

-- functionSpecifierC

-- alignmentSpecifierC

declaratorC = do
  popt pointerC
  directDeclaratorC

directDeclaratorC = do
  paltv [
      identifierC,
      do
        pchar '(' 
        declaratorC
        pchar ')'
    ]
  pkleene (paltv [
      do
        pchar '[' 
        paltv [
            do
              popt typeQualifierListC
              popt assignmentExpressionC,
            do
              pstr "static"
              popt typeQualifierListC
              assignmentExpressionC,
            do
              typeQualifierListC
              pstr "static"
              assignmentExpressionC,
            do
              popt typeQualifierListC
              pchar '*'
          ]
        pchar ']',
      do 
        pchar '('
        paltv [
            parameterTypeListC,
            popt identifierListC
          ] 
        pchar ')'
    ])
  

pointerC = do
  pchar '*'
  popt typeQualifierListC
  popt pointerC

typeQualifierListC = pkleene typeQualifierC

-- parameterTypeListC

-- parameterListC

-- parameterDeclarationC

-- identifierListC

typeNameC = do
  specifierQualifierListC
  popt abstractDeclaratorC

abstractDeclaratorC = paltv [
    do
      opt pointerC 
      directAbstractDeclaratorC,
    pointerC
  ]

directAbstractDeclaratorC = do
  popt (do
    pchar '('
    abstractDeclaratorC
    pchar ')')
  pkleene (
      paltv [
        do
          pchar '['
          paltv [
              popt typeQualifierListC,
              popt assignmentExpressionC,
              do
                pstr "static"
                popt typeQualifierListC
                assignmentExpressionC,
              do
                popt typeQualifierListC
                pstr "static"
                assignmentExpressionC,
              pstr '*'
            ]
          pchar ']',
        do
          pchar '('
          popt parameterTypeListC 
          pchar ')'
      ]
    )


-- typedefNameC

-- initializerC

-- initializerListC

-- designationC

-- designatorListC

-- designatorC

-- staticAssertDeclarationC

-- A.2.3 Statements

-- statementC

-- labeledStatementC

-- compoundStatementC

-- blockItemListC

-- blockItemC

-- expressionStatementC

-- selectionStatementC

-- iterationStatementC

-- jumpStatementC

-- A.2.4 External Definitions

-- translationUnitC

-- externalDeclarationC

-- functionDefinitionC

-- declarationListC

-- A.3 Preprocessing Directives

-- preprocessingFileC

-- groupC

-- groupPartC

-- ifSectionC

-- elifGroupsC

-- elifGroupC

-- elseGroupC

-- endifLineC

-- controlLineC

-- textLineC

-- nonDirectiveC

-- lparenC

-- replacementListC

-- ppTokensC

-- newLineC