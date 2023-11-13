{-# LANGUAGE TemplateHaskell #-}

module A23Statements where

import A21ExpressionsA22Declarations
import GrammarTypes
import Parsing
import Tokenizer
import TokenizerHelpers

-- A.2.3 Statements

statementC =
  getnode $
    paltv
      [ StatementLabeledStatementC <$> labeledStatementC,
        StatementCompoundStatementC <$> compoundStatementC,
        StatementExpressionStatementC <$> expressionStatementC,
        StatementSelectionStatementC <$> selectionStatementC,
        StatementIterationStatementC <$> iterationStatementC,
        StatementJumpStatementC <$> jumpStatementC
      ]

labeledStatementC =
  getnode $
    paltv
      [ do
          ident <- identifierT
          $(punct ":")
          stmt <- statementC
          pure $ LabeledStatementLabelC ident stmt,
        do
          kw KeywordCase
          caseValue <- constantExpressionC
          $(punct ":")
          stmt <- statementC
          pure $ LabeledStatementCaseC caseValue stmt,
        do
          kw KeywordDefault
          $(punct ":")
          LabeledStatementDefaultC
            <$> statementC
      ]

compoundStatementC = getnode $ do
  $(punct "{")
  bil <- popt blockItemListC
  $(punct "}")
  pure $ CompoundStatementC bil

blockItemListC = getnode $ BlockItemListC <$> poneormore blockItemC

blockItemC =
  getnode $
    paltv
      [ BlockItemDeclarationC <$> declarationC,
        BlockItemStatementC <$> statementC
      ]

expressionStatementC = getnode $ do
  expr <- popt expressionC
  $(punct ";")
  pure $ ExpressionStatementC expr

selectionStatementC =
  getnode $
    paltv
      [ do
          kw KeywordIf
          $(punct "(")
          expr <- expressionC
          $(punct ")")
          stmt <- statementC
          pure $ SelectionStatementIfStatementC expr stmt,
        do
          kw KeywordIf
          $(punct "(")
          expr <- expressionC
          $(punct ")")
          stmt <- statementC
          kw KeywordElse
          elsestmt <- statementC
          pure $ SelectionStatementIfElseStatementC expr stmt elsestmt,
        do
          kw KeywordSwitch
          $(punct "(")
          expr <- expressionC
          $(punct ")")
          stmt <- statementC
          pure $ SelectionStatementSwitchStatementC expr stmt
      ]

iterationStatementC =
  getnode $
    paltv
      [ do
          kw KeywordWhile
          $(punct "(")
          expr <- expressionC
          $(punct ")")
          stmt <- statementC
          pure $ IterationStatementWhileC expr stmt,
        do
          kw KeywordDo
          $(punct "(")
          expr <- expressionC
          $(punct ")")
          kw KeywordWhile
          stmt <- statementC
          pure $ IterationStatementDoWhileC expr stmt,
        do
          kw KeywordFor
          $(punct "(")
          init <- popt expressionC
          cond <- popt expressionC
          inc <- popt expressionC
          $(punct ")")
          stmt <- statementC
          pure $ IterationStatementForC init cond inc stmt,
        do
          kw KeywordDo
          $(punct "(")
          dec <- popt declarationC
          expr1 <- popt expressionC
          expr2 <- popt expressionC
          $(punct ")")
          stmt <- statementC
          pure $ IterationStatementDecForC dec expr1 expr2 stmt
      ]

jumpStatementC =
  getnode $
    paltv
      [ do
          kw KeywordGoto
          ident <- identifierT
          $(punct ";")
          pure $ JumpStatementGotoC ident,
        do
          kw KeywordContinue
          $(punct ";")
          pure $ JumpStatementContinueC,
        do
          kw KeywordBreak
          $(punct ";")
          pure $ JumpStatementBreakC,
        do
          kw KeywordReturn
          expr <- popt expressionC
          $(punct ";")
          pure $ JumpStatementReturnC expr
      ]
