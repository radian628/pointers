module A21ExpressionsA22Declarations where

import A13A14A15
import A16StringLiterals
import Data.Foldable
import Data.Maybe
import GrammarTypes
import GrammarUtils
import Parsing
import Tokenizer

--- A.2.1 Expressions

primaryExpressionC :: Parser (CSTNode PrimaryExpressionC)
primaryExpressionC =
  getnode $
    paltv
      [ PrimaryExpressionIdentifierC <$> identifierC,
        PrimaryExpressionConstantC <$> constantC,
        PrimaryExpressionStringLiteralC <$> stringLiteralC,
        PrimaryExpressionExpressionC
          <$> ( do
                  pchar '('
                  expr <- expressionC
                  pchar ')'
                  pure expr
              ),
        PrimaryExpressionGenericSelectionC <$> genericSelectionC
      ]

genericSelectionC = getnode $ do
  pstr "_Generic"
  pchar '('
  ass <- assignmentExpressionC
  pchar ','
  gen <- genericAssocListC
  pchar ')'
  pure $
    GenericSelectionC
      ass
      gen

genericAssocListC =
  getnode $
    GenericAssocListC
      <$> poneormoreDifferent
        genericAssociationC
        ( do
            pchar ','
            genericAssociationC
        )

genericAssociationC =
  getnode $
    paltv
      [ do
          typename <- typeNameC
          pchar ':'
          ass <- assignmentExpressionC
          pure $ GenericAssociationTypeNameC typename ass,
        do
          pstr "default"
          pchar ':'
          GenericAssociationDefaultC <$> assignmentExpressionC
      ]

postfixExpressionC :: Parser (CSTNode PostfixExpressionC)
postfixExpressionC =
  getnode $
    paltv
      [ do
          expr <- primaryExpressionC
          postfixes <- pkleene postfixExpressionInnerCD
          pure $ PostfixExpressionC expr postfixes,
        do
          pchar '('
          tn <- typeNameC
          pchar ')'
          pchar '{'
          inl <- initializerListC
          pchar '}'
          postfixes <- pkleene postfixExpressionInnerCD
          popt $ pchar ','
          pure $
            PostfixExpressionInnerInitializerListCD
              tn
              inl
              postfixes
      ]

postfixExpressionInnerCD :: Parser (CSTNode PostfixExpressionInnerCD)
postfixExpressionInnerCD =
  getnode $
    paltv
      [ do
          pchar '['
          expr <- expressionC
          pchar ']'
          pure $
            PostfixExpressionInnerArraySubscriptCD
              expr,
        do
          pchar '('
          arglist <- argumentExpressionListC
          pchar ')'
          pure $
            PostfixExpressionInnerFunctionCallCD
              arglist,
        do
          pchar '.'
          PostfixExpressionInnerDotCD
            <$> identifierC,
        do
          pstr "->"
          PostfixExpressionInnerArrowCD
            <$> identifierC,
        do
          op <-
            getnode $
              paltv
                [ PostfixExpressionUnaryOpIncrementCD <$ pstr "++",
                  PostfixExpressionUnaryOpDecrementCD <$ pstr "--"
                ]
          pure $ PostfixExpressionInnerUnaryOpCD op
      ]

argumentExpressionListC = getnode $ do
  initArgExpr <- assignmentExpressionC
  argExprs <- pkleene (fmap snd $ pconcat (pchar ',') $ assignmentExpressionC)
  pure (ArgumentExpressionListC (initArgExpr : argExprs))

unaryExpressionC =
  getnode $
    paltv
      [ UnaryExpressionPostfixC <$> postfixExpressionC,
        do
          op <- unaryOperatorC
          expr <- castExpressionC
          pure $ UnaryExpressionGeneralC op expr,
        do
          pstr "sizeof"
          paltv
            [ UnaryExpressionSizeofC <$> unaryExpressionC,
              do
                pchar '('
                tn <- typeNameC
                pchar ')'
                pure (UnaryExpressionSizeofTypeC tn)
            ],
        do
          pstr "_Alignof"
          pchar '('
          tn <- typeNameC
          pchar ')'
          pure (UnaryExpressionAlignofC tn)
      ]

-- unaryOperatorC = paltv (map pchar "&*+-~!")

unaryOperatorC =
  getnode $
    paltv
      [ UnaryOpPrefixInc <$ pstr "++",
        UnaryOpPrefixDec <$ pstr "--",
        UnaryOpAddressOf <$ pstr "&",
        UnaryOpDereference <$ pstr "*",
        UnaryOpPlus <$ pstr "+",
        UnaryOpMinus <$ pstr "-",
        UnaryOpBitwiseNot <$ pstr "~",
        UnaryOpLogicalNot <$ pstr "!"
      ]

castExpressionC =
  doskip $
    getnode $
      paltv
        [ CastExpressionUnaryExpressionC <$> unaryExpressionC,
          do
            pchar '('
            tn <- typeNameC
            pchar ')'
            cast <- castExpressionC
            pure $ CastExpressionCastC tn cast
        ]

binaryOpHelper ::
  [([Char], BinaryOp)] ->
  Parser (CSTNode BinaryOpExpressionCD) ->
  Parser (CSTNode BinaryOpExpressionCD)
binaryOpHelper operators nextParser =
  doskip $ getnode $ do
    left <- nextParser
    rights <- pkleene $ do
      op <-
        getnode $
          paltv (map (\(str, op) -> op <$ pstr str) operators)
      right <- nextParser
      pure (op, right)
    pure $
      BinaryOpExpressionCD
        -- get rid of redundant nested binary op expressions
        ( case left of
            CSTExpression d pp pp' ss se ->
              case d of
                BinaryOpExpressionCastCD _ -> left
                BinaryOpExpressionCD left' rights' ->
                  if null rights'
                    then -- remember to merge the skip tokens of
                    -- outer and inner exprs!
                      mergeskip left left'
                    else left
            CSTError {} -> left
        )
        rights

multiplicativeExpressionC =
  binaryOpHelper
    [ ("*", BinaryOpMul),
      ("/", BinaryOpDiv),
      ("%", BinaryOpRemainder)
    ]
    $ (getnode $ BinaryOpExpressionCastCD <$> castExpressionC)

additiveExpressionC =
  binaryOpHelper
    [ ("+", BinaryOpAdd),
      ("-", BinaryOpSub)
    ]
    multiplicativeExpressionC

shiftExpressionC =
  binaryOpHelper
    [ ("<<", BinaryOpBitshiftLeft),
      (">>", BinaryOpBitshiftRight)
    ]
    additiveExpressionC

relationalExpressionC =
  binaryOpHelper
    [ ("<", BinaryOpLessThan),
      (">", BinaryOpGreaterThan),
      ("<=", BinaryOpLesserEq),
      (">=", BinaryOpGreaterEq)
    ]
    shiftExpressionC

equalityExpressionC =
  binaryOpHelper
    [ ("==", BinaryOpEqualTo),
      ("!=", BinaryOpNotEqualTo)
    ]
    relationalExpressionC

andExpressionC =
  binaryOpHelper [("&", BinaryOpBitwiseAnd)] equalityExpressionC

exclusiveOrExpressionC =
  binaryOpHelper [("^", BinaryOpBitwiseXor)] andExpressionC

inclusiveOrExpressionC =
  binaryOpHelper [("|", BinaryOpBitwiseOr)] exclusiveOrExpressionC

logicalAndExpressionC =
  binaryOpHelper [("&&", BinaryOpLogicalAnd)] inclusiveOrExpressionC

logicalOrExpressionC =
  binaryOpHelper [("||", BinaryOpLogicalOr)] logicalAndExpressionC

conditionalExpressionC =
  getnode $
    paltv
      [ ConditionalExpressionBinaryOpC <$> logicalOrExpressionC,
        do
          cond <- logicalOrExpressionC
          pchar '?'
          iftrue <- expressionC
          pchar ':'
          iffalse <- conditionalExpressionC
          pure $
            ConditionalExpressionTernaryC
              cond
              iftrue
              iffalse
      ]

assignmentExpressionC :: Parser (CSTNode AssignmentExpressionC)
assignmentExpressionC =
  getnode $
    paltv
      [ AssignmentExpressionConditionalExpressionC
          <$> conditionalExpressionC,
        do
          u <- unaryExpressionC
          ao <- assignmentOperatorC
          ae <- assignmentExpressionC
          pure $
            AssignmentExpressionBinaryOpExpressionC
              u
              ao
              ae
      ]

assignmentOperatorC =
  getnode $
    paltv $
      fmap
        (\(parser, v) -> v <$ parser)
        [ (pstr "=", AssignmentOperatorEqualsC),
          (pstr "*=", AssignmentOperatorMulC),
          (pstr "/=", AssignmentOperatorDivC),
          (pstr "%=", AssignmentOperatorModC),
          (pstr "+=", AssignmentOperatorAddC),
          (pstr "-=", AssignmentOperatorSubC),
          (pstr "<<=", AssignmentOperatorLeftShiftC),
          (pstr ">>=", AssignmentOperatorRightShiftC),
          (pstr "&=", AssignmentOperatorBitwiseAndC),
          (pstr "^=", AssignmentOperatorBitwiseXorC),
          (pstr "|=", AssignmentOperatorBitwiseOrC)
        ]

expressionC =
  getnode $
    ExpressionC
      <$> poneormoreDifferent
        assignmentExpressionC
        ( do
            pchar ','
            assignmentExpressionC
        )

constantExpressionC = conditionalExpressionC

-- A.2.2 Declarations

-- declarationC

declarationSpecifiersC =
  getnode $
    DeclarationSpecifiersC <$> poneormore declarationSpecifierCD

declarationSpecifierCD =
  getnode $
    paltv
      [ DeclarationSpecifierStorageClassSpecifierCD <$> storageClassSpecifierC,
        DeclarationSpecifierTypeSpecifierCD <$> typeSpecifierC,
        DeclarationSpecifierTypeQualifierCD <$> typeQualifierC,
        DeclarationSpecifierFunctionSpecifierCD <$> functionSpecifierC,
        DeclarationSpecifierAlignmentSpecifierCD <$> alignmentSpecifierC
      ]

-- initDeclaratorListC

-- initDeclaratorC

storageClassSpecifierC =
  getnode $
    paltv $
      map
        (\(str, spec) -> fmap (const spec) (pstr str))
        [ ("typedef", StorageClassSpecifierTypeDefC),
          ("extern", StorageClassSpecifierExternC),
          ("static", StorageClassSpecifierStaticC),
          ("_Thread_local", StorageClassSpecifierThreadLocalC),
          ("auto", StorageClassSpecifierAutoC),
          ("register", StorageClassSpecifierRegisterC)
        ]

typeSpecifierC =
  getnode $
    paltv
      ( map
          ( \(str, spec) -> fmap (const spec) (pstr str)
          )
          [ ("void", TypeSpecifierVoidC),
            ("char", TypeSpecifierCharC),
            ("short", TypeSpecifierShortC),
            ("int", TypeSpecifierIntC),
            ("long", TypeSpecifierLongC),
            ("float", TypeSpecifierFloatC),
            ("double", TypeSpecifierDoubleC),
            ("signed", TypeSpecifierSignedC),
            ("unsigned", TypeSpecifierUnsignedC),
            ("_Bool", TypeSpecifierBoolC),
            ("_Complex", TypeSpecifierComplexC)
          ]
          ++ [ TypeSpecifierAtomicC <$> atomicTypeSpecifierC,
               TypeSpecifierStructOrUnionC <$> structOrUnionSpecifierC,
               TypeSpecifierEnumSpecifierC <$> enumSpecifierC,
               TypeSpecifierTypedefNameC <$> typedefNameC
             ]
      )

structOrUnionSpecifierC =
  getnode $
    paltv
      [ do
          sou <- structOrUnionC
          name <- popt identifierC
          pchar '{'
          fields <- structDeclarationListC
          pchar '}'
          pure $
            StructOrUnionSpecifierWithDecListC
              sou
              name
              fields,
        do
          sou <- structOrUnionC
          name <- identifierC
          pure $
            StructOrUnionSpecifierC
              sou
              name
      ]

structOrUnionC =
  getnode $
    paltv
      [ StructOrUnionStructC <$ pstr "struct",
        StructOrUnionUnionC <$ pstr "union"
      ]

structDeclarationListC =
  getnode $
    StructDeclarationListC
      <$> poneormore structDeclarationC

structDeclarationC =
  getnode $
    paltv
      [ do
          specqual <- specifierQualifierListC
          declarators <- popt structDeclaratorListC
          pchar ';'
          pure $
            StructDeclarationC
              specqual
              declarators,
        StructDeclarationStaticAssertC <$> staticAssertDeclarationC
      ]

specifierQualifierListC =
  getnode $
    SpecifierQualifierListC
      <$> ( poneormore $
              paltv
                [ fmap Left typeSpecifierC,
                  fmap Right typeQualifierC
                ]
          )

structDeclaratorListC =
  getnode $
    StructDeclaratorListC
      <$> poneormoreDifferent
        structDeclaratorC
        ( do
            pchar ','
            structDeclaratorC
        )

structDeclaratorC =
  getnode $
    paltv
      [ do
          dec <- popt declaratorC
          expr <- constantExpressionC
          pure $ StructDeclaratorWithExprC dec expr,
        do
          dec <- declaratorC
          pure $ StructDeclaratorC dec
      ]

enumSpecifierC = getnode $ do
  pstr "enum"
  paltv
    [ EnumSpecifierC <$> identifierC,
      do
        name <- popt identifierC
        pchar '{'
        enumlist <- enumeratorListC
        popt $ pchar ','
        pchar '}'
        pure $ EnumSpecifierWithDataC name enumlist
    ]

enumeratorListC = getnode $ do
  EnumeratorListC
    <$> poneormoreDifferent
      enumeratorC
      ( do
          pchar ','
          enumeratorC
      )

enumeratorC =
  getnode $
    paltv
      [ EnumeratorC <$> enumerationConstantC,
        do
          enumconst <- enumerationConstantC
          pchar '='
          expr <- constantExpressionC
          pure $ EnumeratorAssignmentC enumconst expr
      ]

atomicTypeSpecifierC :: Parser (CSTNode AtomicTypeSpecifierC)
atomicTypeSpecifierC = getnode $ do
  pstr "_Atomic"
  pchar '('
  typename <- typeNameC
  pchar ')'
  pure $ AtomicTypeSpecifierC typename

typeQualifierC =
  getnode $
    paltv
      ( map
          (\(str, spec) -> fmap (const spec) (pstr str))
          [ ("const", TypeQualifierConstC),
            ("restrict", TypeQualifierRestrictC),
            ("volatile", TypeQualifierVolatileC),
            ("_Atomic", TypeQualifierAtomicC)
          ]
      )

functionSpecifierC =
  getnode $
    paltv
      ( map
          (\(str, spec) -> fmap (const spec) (pstr str))
          [ ("inline", FunctionSpecifierInlineC),
            ("_Noreturn", FunctionSpecifierNoReturnC)
          ]
      )

alignmentSpecifierC :: Parser (CSTNode AlignmentSpecifierC)
alignmentSpecifierC = getnode $ do
  pstr "_Alignas"
  pchar '('
  ret <-
    paltv
      [ AlignmentSpecifierC <$> (Left <$> typeNameC),
        AlignmentSpecifierC <$> Right <$> constantExpressionC
      ]
  pchar ')'
  pure ret

declaratorC = getnode $ do
  ptr <- popt pointerC
  ddc <- directDeclaratorC
  pure $ DeclaratorC ptr ddc

directDeclaratorC = getnode $ do
  dec <-
    paltv
      [ Left <$> identifierC,
        do
          pchar '('
          dec <- Right <$> declaratorC
          pchar ')'
          pure dec
      ]
  suffixes <- pkleene directDeclaratorSuffixCD
  pure $ DirectDeclaratorC dec suffixes

directDeclaratorSuffixCD =
  getnode $
    paltv
      [ do
          pchar '['
          arr <-
            paltv
              [ do
                  tql <- popt typeQualifierListC
                  ass <- popt assignmentExpressionC
                  pure $
                    DirectDeclaratorSuffixArrayC
                      tql
                      ass,
                do
                  pstr "static"
                  tql <- popt typeQualifierListC
                  ass <- assignmentExpressionC
                  pure $
                    DirectDeclaratorSuffixStaticArrayC
                      tql
                      ass,
                do
                  tql <- typeQualifierListC
                  pstr "static"
                  ass <- assignmentExpressionC
                  pure $
                    DirectDeclaratorSuffixStaticArrayC
                      (Just tql)
                      ass,
                do
                  tql <- popt typeQualifierListC
                  pchar '*'
                  pure $
                    DirectDeclaratorSuffixPointerArrayC
                      tql
              ]
          pchar ']'
          pure arr,
        do
          pchar '('
          ptl <- parameterTypeListC
          pchar ')'
          pure $
            DirectDeclaratorSuffixParamTypeListC
              ptl,
        do
          pchar '('
          idl <- popt identifierListC
          pchar ')'
          pure $
            DirectDeclaratorSuffixIdentifierListC
              idl
      ]

pointerC =
  getnode $
    PointerC
      <$> ( pkleene $ do
              pchar '*'
              pointerElementCD
          )

pointerElementCD = getnode $ PointerElementCD <$> popt typeQualifierListC

typeQualifierListC = getnode $ TypeQualifierListC <$> poneormore typeQualifierC

parameterTypeListC = getnode $ do
  plist <- parameterListC
  variadic <-
    popt
      ( do
          pchar ','
          pstr "..."
      )
  pure $ ParameterTypeListC plist (isJust variadic)

parameterListC :: Parser (CSTNode ParameterListC)
parameterListC =
  getnode $
    ParameterListC
      <$> poneormoreDifferent
        parameterDeclarationC
        ( do
            pchar ','
            parameterDeclarationC
        )

parameterDeclarationC = getnode $ do
  declspec <- declarationSpecifiersC
  declarator <-
    paltv
      [ Left <$> declaratorC,
        Right <$> popt abstractDeclaratorC
      ]
  pure $ ParameterDeclarationC declspec declarator

identifierListC =
  getnode $
    IdentifierListC
      <$> poneormoreDifferent
        identifierC
        ( do
            pchar ','
            identifierC
        )

typeNameC :: Parser (CSTNode TypeNameC)
typeNameC = getnode $ do
  specQualList <- specifierQualifierListC
  absdec <- popt abstractDeclaratorC
  pure $ TypeNameC specQualList absdec

abstractDeclaratorC =
  getnode $
    paltv
      [ AbstractDeclaratorPointerC <$> pointerC,
        do
          ptr <- popt pointerC
          dad <- directAbstractDeclaratorC
          pure $
            AbstractDeclaratorDirectC
              ptr
              dad
      ]

directAbstractDeclaratorC = getnode $ do
  ad <-
    popt
      ( do
          pchar '('
          ad <- abstractDeclaratorC
          pchar ')'
          pure ad
      )
  elems <- pkleene directAbstractDeclaratorElementCD
  pure $ DirectAbstractDeclaratorC ad elems

directAbstractDeclaratorElementCD =
  getnode $
    paltv
      [ do
          pchar '['
          tql <- popt typeQualifierListC
          ass <- popt assignmentExpressionC
          pchar ']'
          pure $ DirectAbstractDeclaratorElementArray tql ass,
        do
          pchar '['
          pstr "static"
          tql <- popt typeQualifierListC
          ass <- assignmentExpressionC
          pchar ']'
          pure $ DirectAbstractDeclaratorStaticArray tql ass,
        do
          pchar '['
          tql <- typeQualifierListC
          pstr "static"
          ass <- assignmentExpressionC
          pchar ']'
          pure $
            DirectAbstractDeclaratorStaticArray (Just tql) ass,
        do
          pchar '['
          pchar '*'
          pchar ']'
          pure DirectAbstractDeclaratorStarArray,
        do
          pchar '('
          ptl <- popt parameterTypeListC
          pchar ')'
          pure $
            DirectAbstractDeclaratorParameterList
              ptl
      ]

typedefNameC = identifierC

initializerC =
  paltv
    [ getnode $ InitializerAssignmentC <$> assignmentExpressionC,
      getnode $ do
        pchar '{'
        init <- initializerListC
        popt $ pchar ','
        pchar '}'
        pure (InitializerInitializerListC init)
    ]

initializerListC =
  getnode $
    InitializerListC
      <$> poneormoreDifferent
        ( do
            desg <- popt designationC
            init <- initializerC
            pure (desg, init)
        )
        ( do
            pchar ','
            desg <- popt designationC
            init <- initializerC
            pure (desg, init)
        )

designationC = getnode $ do
  dl <- DesignationC <$> designatorListC
  pchar '='
  pure dl

designatorListC =
  getnode $
    DesignatorListC <$> poneormore designatorC

designatorC =
  paltv
    [ getnode $ do
        pchar '['
        expr <-
          DesignatorArrayC
            <$> (constantExpressionC)
        pchar ']'
        pure expr,
      getnode $ do
        pchar '.'
        DesignatorDotC <$> identifierC
    ]

staticAssertDeclarationC = getnode $ do
  pstr "_Static_assert"
  pchar '('
  expr <- constantExpressionC
  pchar ','
  str <- stringLiteralC
  pchar ')'
  pchar ';'
  pure $ StaticAssertDeclarationC expr str