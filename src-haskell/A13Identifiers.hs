module A13Identifiers where

import A14UniversalCharacterNames
import Data.Char
import GrammarTypes
import GrammarUtils
import Parsing

--- A.1.3 Identifiers

identifierC = do
  init <- identifierNondigitC
  rest <- pkleene (paltv [digitC, identifierNondigitC])
  pure (init : rest)

identifierExprC =
  wrapWithCSTExpression
    ( (\i -> IdentifierExpr {name = i}) <$> identifierC
    )

identifierNondigitC :: Parser Char
identifierNondigitC =
  paltv
    [ nonDigitC,
      universalCharacterNameC
    ]

inBetween x min max = x >= min && x <= max

nonDigitC = pfn (\c -> inBetween (ord c) 65 90 || inBetween (ord c) 97 122 || c == '_')

digitC :: Parser Char
digitC = pfn isDigit
