module GrammarUtils where

import Data.Char (chr, isAlpha, isDigit, ord)
import Data.Foldable (toList)
import Data.Maybe (fromMaybe, isJust)
import Data.Text.Lazy.Builder.Int (hexadecimal)
import Data.Void (Void)
import GHC.Real ((%))
import GrammarTypes
import Parsing

data SkipTokenC
  = SkipTokenWhitespaceC [Char]
  | SkipTokenLineCommentC [Char]
  | SkipTokenBlockCommentC [Char]

doskip p = do
  pkleene skipC
  d <- p
  pkleene skipC
  pure d

skipC =
  getnode $
    paltv
      [ SkipTokenWhitespaceC <$> pstr " ",
        SkipTokenWhitespaceC <$> pstr "\t",
        SkipTokenWhitespaceC <$> pstr "\r",
        SkipTokenWhitespaceC <$> pstr "\n",
        do
          pstr "//"
          SkipTokenLineCommentC <$> pkleene (pfn (/= '\n'))
          -- TODO: proper multi-line comments
          -- do
          --   pstr "/*"
      ]
