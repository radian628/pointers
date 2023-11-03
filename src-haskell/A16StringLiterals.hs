module A16StringLiterals where

import Data.Maybe
import GrammarTypes
import Parsing

--- A.1.6 String Literals

stringLiteralC = do
  prefix <- fmap (fromMaybe CharC) (popt encodingPrefixC)
  pchar '"'
  chars <- sCharSequenceC
  pchar '"'
  pure
    StringLiteralExpr
      { encodingType = prefix,
        stringData = chars
      }

encodingPrefixC =
  paltv
    [ Utf8 <$ pstr "u8",
      Char16T <$ pstr "u",
      Char32T <$ pstr "U",
      WcharT <$ pstr "L"
    ]

sCharSequenceC = pkleene sCharC

sCharC = pfn (\c -> c /= '"' && c /= '\\' && c /= '\n')
