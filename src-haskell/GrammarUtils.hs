module GrammarUtils where

import Data.Char (chr, isAlpha, isDigit, ord)
import Data.Foldable (toList)
import Data.Maybe (fromMaybe, isJust)
import Data.Text.Lazy.Builder.Int (hexadecimal)
import Data.Void (Void)
import GHC.Real ((%))
import GrammarTypes
import Parsing

wrapWithCSTExpression :: Parser CSTExpressionData -> Parser CSTExpression
wrapWithCSTExpression parser = do
  start <- getpp
  exprData <- parser
  end <- getpp
  pure
    CSTExpression
      { exprData,
        start,
        end
      }

-- TODO: Probably find a better way of doing this
wrapWithMaybeCSTExpression parser = do
  start <- getpp
  exprDataMaybe <- parser
  end <- getpp
  case exprDataMaybe of
    Just expr ->
      pure
        ( Just
            CSTExpression
              { exprData = expr,
                start,
                end
              }
        )
    Nothing -> pure Nothing

--- C grammar lifted directly from https://open-std.org/jtc1/sc22/wg14/www/docs/n1570.pdf

--- A.2 Phrase structure grammar
