module A21Expressions where

import A13Identifiers
import A15Constants
import A16StringLiterals
import Data.Foldable
import GrammarTypes
import GrammarUtils
import Parsing

--- A.2.1 Expressions

primaryExpressionC :: Parser CSTExpressionData
primaryExpressionC =
  paltv
    [ fmap (\name -> IdentifierExpr {name}) identifierC,
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
  ass <- wrapWithCSTExpression assignmentExpressionC
  pchar ','
  gen <- genericAssocListC
  pchar ')'
  pure
    GenericSelectionExpr
      { assignment = ass,
        associations = gen
      }

genericAssocListC = do
  genericAssociationC
  pkleene
    ( do
        (pchar ',')
        genericAssociationC
    )

genericAssociationC =
  paltv
    [ do
        typename <- typeNameC
        pchar ':'
        ass <- wrapWithCSTExpression assignmentExpressionC
        pure (Just typename, ass),
      do
        pstr "default"
        pchar ':'
        ass <- wrapWithCSTExpression assignmentExpressionC
        pure (Nothing, ass)
    ]

postfixExpressionC :: Parser CSTExpressionData
postfixExpressionC =
  paltv
    [ primaryExpressionC,
      do
        left <- wrapWithCSTExpression postfixExpressionC
        pchar '['
        right <- wrapWithCSTExpression expressionC
        pchar ']'
        pure
          BinaryOperatorExpr
            { op = ArraySubscript,
              left,
              right
            },
      do
        left <- wrapWithCSTExpression postfixExpressionC
        paltv
          [ do
              pchar '('
              right <- popt $ wrapWithCSTExpression argumentExpressionListC
              pchar ')'
              pure
                FunctionCallExpr
                  { fnName = left,
                    fnArgs = right
                  },
            do
              pchar '.'
              ident <- identifierExprC
              pure
                BinaryOperatorExpr
                  { op = MemberAccess,
                    left,
                    right = ident
                  },
            do
              pstr "->"
              ident <- identifierExprC
              pure
                BinaryOperatorExpr
                  { op = PointerMemberAccess,
                    left,
                    right = ident
                  },
            UnaryOperatorExpr {unaryOp = PostfixInc, unaryOperand = left}
              <$ pstr
                "++",
            UnaryOperatorExpr {unaryOp = PostfixDec, unaryOperand = left}
              <$ pstr
                "--"
          ],
      do
        pchar '('
        tn <- typeNameC
        pchar ')'
        pchar '{'
        il <- initializerListC
        popt $ pchar ','
        pchar '}'
        pure
          InitializerListExpression
            { initializerList = il,
              initializerListTypeName = tn
            }
    ]

argumentExpressionListC :: Parser CSTExpressionData
argumentExpressionListC = do
  initArgExpr <- wrapWithCSTExpression assignmentExpressionC
  argExprs <- pkleene (fmap snd $ pconcat (pchar ',') $ wrapWithCSTExpression assignmentExpressionC)
  pure (ArgumentExpressionListExpr (initArgExpr : argExprs))

unaryExpressionC =
  paltv
    [ postfixExpressionC,
      do
        paltv [pstr "++", pstr "--"]
        unaryExpressionC,
      do
        unaryOperatorC
        castExpressionC,
      do
        pstr "sizeof"
        paltv
          [ unaryExpressionC,
            do
              pchar '('
              tn <- typeNameC
              pchar ')'
              pure (SizeofExpr tn)
          ],
      do
        pstr "_Alignof"
        pchar '('
        tn <- typeNameC
        pchar ')'
        pure (AlignofExpr tn)
    ]

unaryOperatorC = paltv (map pchar "&*+-~!")

castExpressionC =
  paltv
    [ unaryExpressionC,
      do
        pchar '('
        typeNameC
        pchar ')'
        castExpressionC
    ]

binaryOpHelper :: [[Char]] -> Parser a -> Parser a -> Parser a
binaryOpHelper operators thisParser nextParser =
  paltv
    [ nextParser,
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

conditionalExpressionC :: Parser CSTExpressionData
conditionalExpressionC =
  paltv
    [ logicalOrExpressionC,
      do
        logicalOrExpressionC
        pchar '?'
        expressionC
        pchar ':'
        conditionalExpressionC
    ]

assignmentExpressionC :: Parser CSTExpressionData
assignmentExpressionC =
  paltv
    [ conditionalExpressionC,
      do
        unaryExpressionC
        assignmentOperatorC
        assignmentExpressionC
    ]

assignmentOperatorC =
  paltv
    [ pstr "=",
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

expressionC =
  paltv
    [ assignmentExpressionC,
      do
        expressionC
        pchar ','
        assignmentExpressionC
    ]

constantExpressionC = conditionalExpressionC

-- A.2.2 Declarations

-- declarationC

declarationSpecifiersC :: Parser CSTDeclSpec
declarationSpecifiersC = do
  specifierOrQualifier <-
    paltv
      [ StorageClassSpecifierDSFH <$> storageClassSpecifierC,
        TypeSpecifierDSFH <$> typeSpecifierC,
        TypeQualifierDSFH <$> typeQualifierC,
        FunctionSpecifierDSFH <$> functionSpecifierC,
        AlignmentSpecifierDSFH <$> alignmentSpecifierC
      ]
  declspec <- popt declarationSpecifiersC
  pure $ CSTDeclSpec (specifierOrQualifier, declspec)

-- initDeclaratorListC

-- initDeclaratorC

storageClassSpecifierC =
  paltv $
    map
      (\(str, spec) -> fmap (const spec) (pstr str))
      [ ("typedef", SCSTypedef),
        ("extern", SCSExtern),
        ("static", SCSStatic),
        ("_Thread_local", SCSThreadLocal),
        ("auto", SCSAuto),
        ("register", SCSRegister)
      ]

typeSpecifierC :: Parser TypeSpecifier
typeSpecifierC =
  paltv
    ( map
        ( \(str, spec) -> fmap (const spec) (pstr str)
        )
        [ ("void", TSVoid),
          ("char", TSChar),
          ("short", TSShort),
          ("int", TSInt),
          ("long", TSLong),
          ("float", TSFloat),
          ("double", TSDouble),
          ("signed", TSSigned),
          ("unsigned", TSUnsigned),
          ("_Bool", TSBool),
          ("_Complex", TSComplex)
        ]
        ++ [ atomicTypeSpecifierC,
             TSStuctOrUnion <$> structOrUnionSpecifierC,
             TSEnumSpecifier <$> enumSpecifierC,
             TSTypedefName <$> typedefNameC
           ]
    )

structOrUnionSpecifierC =
  paltv
    [ do
        sou <- structOrUnionC
        name <- popt identifierC
        pchar '{'
        fields <- structDeclarationListC
        pchar '}'
        pure
          StructOrUnionSpecifier
            { structOrUnionName = name,
              structOrUnionType = sou,
              structOrUnionFields = Just fields
            },
      do
        sou <- structOrUnionC
        name <- identifierC
        pure
          StructOrUnionSpecifier
            { structOrUnionType = sou,
              structOrUnionName = Just name,
              structOrUnionFields = Nothing
            }
    ]

structOrUnionC =
  paltv
    [ StructType <$ pstr "struct",
      UnionType <$ pstr "union"
    ]

structDeclarationListC = pkleene structDeclarationC

structDeclarationC =
  paltv
    [ do
        specqual <- specifierQualifierListC
        declarators <- popt structDeclaratorListC
        pchar ';'
        pure
          StructDeclaration
            { fieldType = specqual,
              fieldDeclarators = declarators
            },
      StructDeclarationStaticAssert <$> staticAssertDeclarationC
    ]

specifierQualifierListC =
  pkleene $
    paltv
      [ fmap IsTypeSpecifier typeSpecifierC,
        fmap IsTypeQualifier typeQualifierC
      ]

structDeclaratorListC = do
  h <- structDeclaratorC
  t <-
    pkleene
      ( do
          pchar ','
          structDeclaratorC
      )
  pure (h : t)

structDeclaratorC =
  paltv
    [ do
        dec <- popt declaratorC
        expr <- wrapWithCSTExpression constantExpressionC
        pure $ StructDeclarator dec (Just expr),
      do
        dec <- declaratorC
        pure $ StructDeclarator (Just dec) Nothing
    ]

enumSpecifierC = do
  pstr "enum"
  paltv
    [ EnumSpecifierDeclaration <$> identifierC,
      do
        name <- popt identifierC
        pchar '{'
        enumlist <- enumeratorListC
        popt $ pchar ','
        pchar '}'
        pure $ EnumSpecifierDefinition name enumlist
    ]

enumeratorListC = do
  h <- enumeratorC
  t <-
    pkleene
      ( do
          pchar ','
          enumeratorC
      )
  pure (h : t)

enumeratorC =
  paltv
    [ (`Enumerator` Nothing) <$> enumerationConstantC,
      do
        enumconst <- enumerationConstantC
        pchar '='
        expr <- wrapWithCSTExpression constantExpressionC
        pure $ Enumerator enumconst (Just expr)
    ]

atomicTypeSpecifierC = do
  pstr "_Atomic"
  pchar '('
  typename <- typeNameC
  pchar ')'
  pure $ TSAtomic typename

typeQualifierC :: Parser TypeQualifier
typeQualifierC =
  paltv
    ( map
        (\(str, spec) -> fmap (const spec) (pstr str))
        [ ("const", TQConst),
          ("restrict", TQRestrict),
          ("volatile", TQVolatile),
          ("_Atomic", TQAtomic)
        ]
    )

functionSpecifierC =
  paltv
    ( map
        (\(str, spec) -> fmap (const spec) (pstr str))
        [ ("inline", FSInline),
          ("_Noreturn", FSNoReturn)
        ]
    )

alignmentSpecifierC = do
  pstr "_Alignas"
  pchar '('
  ret <-
    paltv
      [ ASTypeName <$> typeNameC,
        ASConstantExpression <$> wrapWithCSTExpression constantExpressionC
      ]
  pchar ')'
  pure ret

declaratorC = do
  popt pointerC
  directDeclaratorC

directDeclaratorC = do
  paltv
    [ identifierC,
      do
        pchar '('
        declaratorC
        pchar ')'
    ]
  pkleene
    ( paltv
        [ do
            pchar '['
            paltv
              [ do
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
            paltv
              [ parameterTypeListC,
                popt identifierListC
              ]
            pchar ')'
        ]
    )

pointerC =
  pkleene $ do
    pchar '*'
    concat . toList <$> popt typeQualifierListC

typeQualifierListC = pkleene typeQualifierC

parameterTypeListC = do
  plist <- parameterListC
  variadic <-
    popt
      ( do
          pchar ','
          pstr "..."
      )
  pure (plist, isJust variadic)

parameterListC = do
  firstDec <- parameterDeclarationC
  nextDecs <-
    pkleene
      ( do
          pchar ','
          parameterDeclarationC
      )
  pure firstDec : nextDecs

parameterDeclarationC = do
  declspec <- declarationSpecifiersC
  idk <- -- TODO: rename
    paltv
      [ Left <$> declaratorC,
        Right <$> popt abstractDeclaratorC
      ]
  pure (declspec, idk)

-- identifierListC

typeNameC :: Parser CSTTypeName
typeNameC = do
  specQualList <- specifierQualifierListC
  popt abstractDeclaratorC

abstractDeclaratorC :: Parser AbstractDeclarator
abstractDeclaratorC =
  paltv
    [ do
        ptr <- popt pointerC
        dad <- directAbstractDeclaratorC
        pure $ AbstractDeclarator (ptr, Just dad),
      fmap (\ptr -> AbstractDeclarator (Just ptr, Nothing)) pointerC
    ]

directAbstractDeclaratorC :: Parser [DirectAbstractDeclaratorElement]
directAbstractDeclaratorC = do
  popt
    ( do
        pchar '('
        ad <- abstractDeclaratorC
        pchar ')'
        pure ad
    )
  patleast
    ( paltv
        [ do
            pchar '['
            ret <-
              paltv
                [ ( \ql ->
                      ArrayDADE Nothing (concat . toList $ ql) False
                  )
                    <$> popt typeQualifierListC,
                  ( \ass ->
                      ArrayDADE ass [] False
                  )
                    <$> wrapWithMaybeCSTExpression
                      (popt assignmentExpressionC),
                  do
                    pstr "static"
                    tql <- concat . toList <$> popt typeQualifierListC
                    ass <- wrapWithCSTExpression assignmentExpressionC
                    pure $ ArrayDADE (Just ass) tql True,
                  do
                    tql <- concat . toList <$> popt typeQualifierListC
                    pstr "static"
                    ass <- wrapWithCSTExpression assignmentExpressionC
                    pure $ ArrayDADE (Just ass) tql True,
                  AsteriskDADE <$ pchar '*'
                ]
            pchar ']'
            pure ret,
          do
            pchar '('
            parameterList <- popt parameterTypeListC
            pchar ')'
            pure parameterList
        ]
    )
    1

typedefNameC = identifierC

initializerC =
  paltv
    [ InitializerInitializer <$> wrapWithCSTExpression assignmentExpressionC,
      do
        pchar '{'
        init <- initializerListC
        popt $ pchar ','
        pchar '}'
        pure (InitializerListInitializer init)
    ]

initializerListC =
  InitializerList
    <$> pkleene
      ( do
          desg <- popt designationC
          init <- initializerC
          pure (desg, init)
      )

designationC = do
  dl <- designatorListC
  pchar '='
  pure dl

-- TODO: this

designatorListC = Designation <$> patleast designatorC 1

designatorC :: Parser Designator
designatorC =
  paltv
    [ do
        pchar '['
        expr <- ExpressionDesignator <$> wrapWithCSTExpression constantExpressionC
        pchar ']'
        pure expr,
      do
        pchar '.'
        IdentifierDesignator <$> identifierC
    ]

staticAssertDeclarationC = do
  pstr "_Static_assert"
  pchar '('
  expr <- wrapWithCSTExpression constantExpressionC
  pchar ','
  str <- stringLiteralC
  pchar ')'
  pchar ';'
  pure $ StaticAssertDeclaration expr str