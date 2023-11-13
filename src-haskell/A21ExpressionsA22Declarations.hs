{-# LANGUAGE TemplateHaskell #-}

module A21ExpressionsA22Declarations where

import A13A14A15
import A16StringLiterals
import Data.Foldable
import Data.Maybe
import GrammarTypes
import GrammarUtils
import Parsing
import Tokenizer
import TokenizerHelpers

--- A.2.1 Expressions

primaryExpressionC :: TokenParser (CSTNode PrimaryExpressionC)
primaryExpressionC =
  getnode $
    paltv
      [ PrimaryExpressionIdentifierC <$> identifierT,
        PrimaryExpressionConstantC <$> constantT,
        PrimaryExpressionStringLiteralC <$> getnode stringLiteralT,
        PrimaryExpressionExpressionC
          <$> ( do
                  punctuatorT PParenOpen
                  expr <- expressionC
                  punctuatorT PParenClosed
                  pure expr
              ),
        PrimaryExpressionGenericSelectionC <$> genericSelectionC
      ]

genericSelectionC = getnode $ do
  kw KeywordGeneric
  $(punct "(")
  ass <- assignmentExpressionC
  $(punct ",")
  gen <- genericAssocListC
  $(punct ")")
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
            $(punct ",")
            genericAssociationC
        )

genericAssociationC =
  getnode $
    paltv
      [ do
          typename <- typeNameC
          punctuatorT PColon
          ass <- assignmentExpressionC
          pure $ GenericAssociationTypeNameC typename ass,
        do
          kw KeywordDefault
          $(punct ":")
          GenericAssociationDefaultC <$> assignmentExpressionC
      ]

postfixExpressionC :: TokenParser (CSTNode PostfixExpressionC)
postfixExpressionC =
  getnode $
    paltv
      [ do
          expr <- primaryExpressionC
          postfixes <- pkleene postfixExpressionInnerCD
          pure $ PostfixExpressionC expr postfixes,
        do
          $(punct "(")
          tn <- typeNameC
          $(punct ")")
          $(punct "{")
          inl <- initializerListC
          $(punct "}")
          postfixes <- pkleene postfixExpressionInnerCD
          popt $ $(punct ",")
          pure $
            PostfixExpressionInnerInitializerListCD
              tn
              inl
              postfixes
      ]

postfixExpressionInnerCD :: TokenParser (CSTNode PostfixExpressionInnerCD)
postfixExpressionInnerCD =
  getnode $
    paltv
      [ do
          $(punct "[")
          expr <- expressionC
          $(punct "]")
          pure $
            PostfixExpressionInnerArraySubscriptCD
              expr,
        do
          $(punct "(")
          arglist <- argumentExpressionListC
          $(punct ")")
          pure $
            PostfixExpressionInnerFunctionCallCD
              arglist,
        do
          $(punct ".")
          PostfixExpressionInnerDotCD
            <$> identifierT,
        do
          $(punct "->")
          PostfixExpressionInnerArrowCD
            <$> identifierT,
        do
          op <-
            getnode $
              paltv
                [ PostfixExpressionUnaryOpIncrementCD <$ $(punct "++"),
                  PostfixExpressionUnaryOpDecrementCD <$ $(punct "--")
                ]
          pure $ PostfixExpressionInnerUnaryOpCD op
      ]

argumentExpressionListC = getnode $ do
  initArgExpr <- assignmentExpressionC
  argExprs <- pkleene (fmap snd $ pconcat $(punct ",") $ assignmentExpressionC)
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
          kw KeywordSizeof
          paltv
            [ UnaryExpressionSizeofC <$> unaryExpressionC,
              do
                $(punct "(")
                tn <- typeNameC
                $(punct ")")
                pure (UnaryExpressionSizeofTypeC tn)
            ],
        do
          kw KeywordSizeof
          $(punct "(")
          tn <- typeNameC
          $(punct ")")
          pure (UnaryExpressionAlignofC tn)
      ]

-- unaryOperatorC = paltv (map pchar "&*+-~!")

unaryOperatorC =
  getnode $
    paltv
      [ UnaryOpPrefixInc <$ $(punct "++"),
        UnaryOpPrefixDec <$ $(punct "--"),
        UnaryOpAddressOf <$ $(punct "&"),
        UnaryOpDereference <$ $(punct "*"),
        UnaryOpPlus <$ $(punct "+"),
        UnaryOpMinus <$ $(punct "-"),
        UnaryOpBitwiseNot <$ $(punct "~"),
        UnaryOpLogicalNot <$ $(punct "!")
      ]

castExpressionC =
  getnode $
    paltv
      [ CastExpressionUnaryExpressionC <$> unaryExpressionC,
        do
          $(punct "(")
          tn <- typeNameC
          $(punct ")")
          cast <- castExpressionC
          pure $ CastExpressionCastC tn cast
      ]

binaryOpHelper ::
  [TokenParser BinaryOp] ->
  TokenParser (CSTNode BinaryOpExpressionCD) ->
  TokenParser (CSTNode BinaryOpExpressionCD)
binaryOpHelper operators nextParser =
  getnode $ do
    left <- nextParser
    rights <- pkleene $ do
      op <-
        getnode $
          paltv operators
      right <- nextParser
      pure (op, right)
    pure $
      BinaryOpExpressionCD
        -- get rid of redundant nested binary op expressions
        ( case left of
            CSTExpression d pp pp' ->
              case d of
                BinaryOpExpressionCastCD _ -> left
                BinaryOpExpressionCD left' rights' ->
                  if null rights'
                    then -- remember to merge the skip tokens of
                    -- outer and inner exprs!
                      left'
                    else left
            CSTError {} -> left
        )
        rights

multiplicativeExpressionC =
  binaryOpHelper
    [ BinaryOpMul <$ $(punct "*"),
      BinaryOpDiv <$ $(punct "/"),
      BinaryOpRemainder <$ $(punct "%")
    ]
    $ (getnode $ BinaryOpExpressionCastCD <$> castExpressionC)

additiveExpressionC =
  binaryOpHelper
    [ BinaryOpAdd <$ $(punct "+"),
      BinaryOpSub <$ $(punct "-")
    ]
    multiplicativeExpressionC

shiftExpressionC =
  binaryOpHelper
    [ BinaryOpBitshiftLeft <$ $(punct "<<"),
      BinaryOpBitshiftRight <$ $(punct ">>")
    ]
    additiveExpressionC

relationalExpressionC =
  binaryOpHelper
    [ BinaryOpLessThan <$ $(punct "<"),
      BinaryOpGreaterThan <$ $(punct ">"),
      BinaryOpLesserEq <$ $(punct "<="),
      BinaryOpGreaterEq <$ $(punct ">=")
    ]
    shiftExpressionC

equalityExpressionC =
  binaryOpHelper
    [ BinaryOpEqualTo <$ $(punct "=="),
      BinaryOpNotEqualTo <$ $(punct "!=")
    ]
    relationalExpressionC

andExpressionC =
  binaryOpHelper
    [ BinaryOpBitwiseAnd <$ $(punct "&")
    ]
    equalityExpressionC

exclusiveOrExpressionC =
  binaryOpHelper
    [ BinaryOpBitwiseXor <$ $(punct "^")
    ]
    andExpressionC

inclusiveOrExpressionC =
  binaryOpHelper
    [ BinaryOpBitwiseOr <$ $(punct "|")
    ]
    exclusiveOrExpressionC

logicalAndExpressionC =
  binaryOpHelper
    [ BinaryOpLogicalAnd <$ $(punct "&&")
    ]
    inclusiveOrExpressionC

logicalOrExpressionC =
  binaryOpHelper
    [ BinaryOpLogicalOr <$ $(punct "||")
    ]
    logicalAndExpressionC

conditionalExpressionC =
  getnode $
    paltv
      [ do
          cond <- logicalOrExpressionC
          $(punct "?")
          iftrue <- expressionC
          $(punct ":")
          iffalse <- conditionalExpressionC
          pure $
            ConditionalExpressionTernaryC
              cond
              iftrue
              iffalse,
        ConditionalExpressionBinaryOpC <$> logicalOrExpressionC
      ]

assignmentExpressionC :: TokenParser (CSTNode AssignmentExpressionC)
assignmentExpressionC =
  getnode $
    paltv
      [ do
          u <- unaryExpressionC
          ao <- assignmentOperatorC
          ae <- assignmentExpressionC
          pure $
            AssignmentExpressionBinaryOpExpressionC
              u
              ao
              ae,
        AssignmentExpressionConditionalExpressionC
          <$> conditionalExpressionC
      ]

assignmentOperatorC =
  getnode $
    paltv $
      fmap
        (\(parser, v) -> v <$ parser)
        [ ($(punct "="), AssignmentOperatorEqualsC),
          ($(punct "*="), AssignmentOperatorMulC),
          ($(punct "/="), AssignmentOperatorDivC),
          ($(punct "%="), AssignmentOperatorModC),
          ($(punct "+="), AssignmentOperatorAddC),
          ($(punct "-="), AssignmentOperatorSubC),
          ($(punct "<<="), AssignmentOperatorLeftShiftC),
          ($(punct ">>="), AssignmentOperatorRightShiftC),
          ($(punct "&="), AssignmentOperatorBitwiseAndC),
          ($(punct "^="), AssignmentOperatorBitwiseXorC),
          ($(punct "|="), AssignmentOperatorBitwiseOrC)
        ]

expressionC =
  getnode $
    ExpressionC
      <$> poneormoreDifferent
        assignmentExpressionC
        ( do
            $(punct ",")
            assignmentExpressionC
        )

constantExpressionC = conditionalExpressionC

-- A.2.2 Declarations

declarationC =
  getnode $
    paltv
      [ do
          declspec <- declarationSpecifiersC
          declist <- popt initDeclaratorListC
          $(punct ";")
          pure $ DeclarationC declspec declist,
        DeclarationStaticAssertC <$> staticAssertDeclarationC
      ]

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

initDeclaratorListC =
  getnode $
    ( InitDeclaratorListC
        <$> poneormoreDifferent
          initDeclaratorC
          ( do
              $(punct ",")
              dec <- initDeclaratorC
              pure $ dec
          )
    )

initDeclaratorC =
  getnode $
    paltv
      [ InitDeclaratorDeclaratorC <$> declaratorC,
        do
          dec <- declaratorC
          $(punct "=")
          init <- initializerC
          pure $ InitDeclaratorAssignmentC dec init
      ]

storageClassSpecifierC =
  getnode $
    paltv $
      map
        (\(str, spec) -> fmap (const spec) (kw str))
        [ (KeywordTypedef, StorageClassSpecifierTypeDefC),
          (KeywordExtern, StorageClassSpecifierExternC),
          (KeywordStatic, StorageClassSpecifierStaticC),
          (KeywordThreadLocal, StorageClassSpecifierThreadLocalC),
          (KeywordAuto, StorageClassSpecifierAutoC),
          (KeywordRegister, StorageClassSpecifierRegisterC)
        ]

typeSpecifierC =
  getnode $
    paltv
      ( map
          ( \(str, spec) -> fmap (const spec) (kw str)
          )
          [ (KeywordVoid, TypeSpecifierVoidC),
            (KeywordChar, TypeSpecifierCharC),
            (KeywordShort, TypeSpecifierShortC),
            (KeywordInt, TypeSpecifierIntC),
            (KeywordLong, TypeSpecifierLongC),
            (KeywordFloat, TypeSpecifierFloatC),
            (KeywordDouble, TypeSpecifierDoubleC),
            (KeywordSigned, TypeSpecifierSignedC),
            (KeywordUnsigned, TypeSpecifierUnsignedC),
            (KeywordBool, TypeSpecifierBoolC),
            (KeywordComplex, TypeSpecifierComplexC)
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
          name <- popt $ identifierT
          $(punct "{")
          fields <- structDeclarationListC
          $(punct "}")
          pure $
            StructOrUnionSpecifierWithDecListC
              sou
              name
              fields,
        do
          sou <- structOrUnionC
          name <- identifierT
          pure $
            StructOrUnionSpecifierC
              sou
              name
      ]

structOrUnionC =
  getnode $
    paltv
      [ StructOrUnionStructC <$ kw KeywordStruct,
        StructOrUnionUnionC <$ kw KeywordUnion
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
          $(punct ";")
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
            $(punct ",")
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
  kw KeywordEnum
  paltv
    [ EnumSpecifierC <$> identifierT,
      do
        name <- popt $ identifierT
        $(punct "{")
        enumlist <- enumeratorListC
        popt $
          $(punct ",")
        $(punct "}")
        pure $ EnumSpecifierWithDataC name enumlist
    ]

enumeratorListC = getnode $ do
  EnumeratorListC
    <$> poneormoreDifferent
      enumeratorC
      ( do
          $(punct ",")
          enumeratorC
      )

enumeratorC =
  getnode $
    paltv
      [ EnumeratorC <$> identifierT,
        do
          enumconst <- identifierT
          $(punct "=")
          expr <- constantExpressionC
          pure $ EnumeratorAssignmentC enumconst expr
      ]

atomicTypeSpecifierC :: TokenParser (CSTNode AtomicTypeSpecifierC)
atomicTypeSpecifierC = getnode $ do
  kw KeywordAtomic
  $(punct "(")
  typename <- typeNameC
  $(punct ")")
  pure $ AtomicTypeSpecifierC typename

typeQualifierC =
  getnode $
    paltv
      ( map
          (\(str, spec) -> fmap (const spec) (kw str))
          [ (KeywordConst, TypeQualifierConstC),
            (KeywordRestrict, TypeQualifierRestrictC),
            (KeywordVolatile, TypeQualifierVolatileC),
            (KeywordAtomic, TypeQualifierAtomicC)
          ]
      )

functionSpecifierC =
  getnode $
    paltv
      ( map
          (\(str, spec) -> fmap (const spec) (kw str))
          [ (KeywordInline, FunctionSpecifierInlineC),
            (KeywordNoreturn, FunctionSpecifierNoReturnC)
          ]
      )

alignmentSpecifierC :: TokenParser (CSTNode AlignmentSpecifierC)
alignmentSpecifierC = getnode $ do
  kw KeywordAlignAs
  $(punct "(")
  ret <-
    paltv
      [ AlignmentSpecifierC <$> (Left <$> typeNameC),
        AlignmentSpecifierC <$> Right <$> constantExpressionC
      ]
  $(punct ")")
  pure ret

declaratorC = getnode $ do
  ptr <- popt pointerC
  ddc <- directDeclaratorC
  pure $ DeclaratorC ptr ddc

directDeclaratorC = getnode $ do
  dec <-
    paltv
      [ Left <$> identifierT,
        do
          $(punct "(")
          dec <- Right <$> declaratorC
          $(punct ")")
          pure dec
      ]
  suffixes <- pkleene directDeclaratorSuffixCD
  pure $ DirectDeclaratorC dec suffixes

directDeclaratorSuffixCD =
  getnode $
    paltv
      [ do
          $(punct "[")
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
                  kw KeywordStatic
                  tql <- popt typeQualifierListC
                  ass <- assignmentExpressionC
                  pure $
                    DirectDeclaratorSuffixStaticArrayC
                      tql
                      ass,
                do
                  tql <- typeQualifierListC
                  kw KeywordStatic
                  ass <- assignmentExpressionC
                  pure $
                    DirectDeclaratorSuffixStaticArrayC
                      (Just tql)
                      ass,
                do
                  tql <- popt typeQualifierListC
                  $(punct "*")
                  pure $
                    DirectDeclaratorSuffixPointerArrayC
                      tql
              ]
          $(punct "]")
          pure arr,
        do
          $(punct "(")
          ptl <- parameterTypeListC
          $(punct ")")
          pure $
            DirectDeclaratorSuffixParamTypeListC
              ptl,
        do
          $(punct "(")
          idl <- popt identifierListC
          $(punct ")")
          pure $
            DirectDeclaratorSuffixIdentifierListC
              idl
      ]

pointerC =
  getnode $
    PointerC
      <$> ( poneormore $ do
              $(punct "*")
              pointerElementCD
          )

pointerElementCD = getnode $ PointerElementCD <$> popt typeQualifierListC

typeQualifierListC = getnode $ TypeQualifierListC <$> poneormore typeQualifierC

parameterTypeListC = getnode $ do
  plist <- parameterListC
  variadic <-
    popt
      ( do
          $(punct ",")
          $(punct "...")
      )
  pure $ ParameterTypeListC plist (isJust variadic)

parameterListC :: TokenParser (CSTNode ParameterListC)
parameterListC =
  getnode $
    ParameterListC
      <$> poneormoreDifferent
        parameterDeclarationC
        ( do
            $(punct ",")
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
        (identifierT)
        ( do
            $(punct ",")
            identifierT
        )

typeNameC :: TokenParser (CSTNode TypeNameC)
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
          $(punct "(")
          ad <- abstractDeclaratorC
          $(punct ")")
          pure ad
      )
  elems <- pkleene directAbstractDeclaratorElementCD
  pure $ DirectAbstractDeclaratorC ad elems

directAbstractDeclaratorElementCD =
  getnode $
    paltv
      [ do
          $(punct "[")
          tql <- popt typeQualifierListC
          ass <- popt assignmentExpressionC
          $(punct "]")
          pure $ DirectAbstractDeclaratorElementArray tql ass,
        do
          $(punct "[")
          kw KeywordStatic
          tql <- popt typeQualifierListC
          ass <- assignmentExpressionC
          $(punct "]")
          pure $ DirectAbstractDeclaratorStaticArray tql ass,
        do
          $(punct "[")
          tql <- typeQualifierListC
          kw KeywordStatic
          ass <- assignmentExpressionC
          $(punct "]")
          pure $
            DirectAbstractDeclaratorStaticArray (Just tql) ass,
        do
          $(punct "[")
          $(punct "*")
          $(punct "]")
          pure DirectAbstractDeclaratorStarArray,
        do
          $(punct "(")
          ptl <- popt parameterTypeListC
          $(punct ")")
          pure $
            DirectAbstractDeclaratorParameterList
              ptl
      ]

typedefNameC = identifierT

initializerC =
  paltv
    [ getnode $ InitializerAssignmentC <$> assignmentExpressionC,
      getnode $ do
        $(punct "{")
        init <- initializerListC
        popt $ $(punct ",")
        $(punct "}")
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
            $(punct ",")
            desg <- popt designationC
            init <- initializerC
            pure (desg, init)
        )

designationC = getnode $ do
  dl <- DesignationC <$> designatorListC
  $(punct "=")
  pure dl

designatorListC =
  getnode $
    DesignatorListC <$> poneormore designatorC

designatorC =
  paltv
    [ getnode $ do
        $(punct "[")
        expr <-
          DesignatorArrayC
            <$> (constantExpressionC)
        $(punct "]")
        pure expr,
      getnode $ do
        $(punct ".")
        DesignatorDotC <$> identifierT
    ]

staticAssertDeclarationC = getnode $ do
  kw KeywordStaticAssert
  $(punct "(")
  expr <- constantExpressionC
  $(punct ",")
  str <- getnode $ stringLiteralT
  $(punct ")")
  $(punct ";")
  pure $ StaticAssertDeclarationC expr str