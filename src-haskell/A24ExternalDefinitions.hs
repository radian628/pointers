{-# LANGUAGE LambdaCase #-}

module A24ExternalDefinitions where

import A21ExpressionsA22Declarations (declarationC, declarationSpecifiersC, declaratorC)
import A23Statements
import Data.Foldable
import Data.List.NonEmpty (nonEmpty)
import GrammarTypes
import Parsing

-- A.2.4 External Definitions

translationUnitC = getnode $ poneormore externalDeclarationC

externalDeclarationC =
  getnode $
    paltv
      [ ExternalDeclarationFunctionDefinitionC <$> functionDefinitionC,
        ExternalDelcarationDeclarationC <$> declarationC
      ]

declSpecWithoutEndCD =
  getnode $
    useInnerNode $
      ( \case
          expr@(CSTExpression (DeclarationSpecifiersC declspecs) _ _) ->
            ( case nonEmpty . init . toList $ declspecs of
                Just l ->
                  CSTExpression
                    ( DeclarationSpecifiersC
                        l
                    )
                    (nodeStart expr)
                    (nodeEnd (head (toList l)))
                Nothing ->
                  CSTError
                    "function call needs declspecs!"
                    (nodeStart expr)
                    (nodeEnd expr)
            )
          expr@CSTError {} -> expr
      )
        <$> declarationSpecifiersC

functionDefinitionWithGivenDeclSpecTypeCD declspecParser =
  do
    declspec <- declspecParser
    declarator <- declaratorC
    declarationList <- popt declarationListC
    stmt <- compoundStatementC
    pure $
      FunctionDefinitionC
        declspec
        declarator
        declarationList
        stmt

functionDefinitionC =
  getnode $
    paltv
      [ functionDefinitionWithGivenDeclSpecTypeCD declarationSpecifiersC,
        functionDefinitionWithGivenDeclSpecTypeCD declSpecWithoutEndCD
      ]

declarationListC = getnode $ (DeclarationListC <$> poneormore declarationC)