module A16StringLiterals where

import Data.Maybe
import GrammarTypes
import GrammarUtils
import Parsing
import Tokenizer

--- A.1.6 String Literals

stringLiteralC = getnode $ do
  prefix <- popt encodingPrefixC
  pchar '"'
  chars <- sCharSequenceC
  pchar '"'
  pure
    ( StringLiteralC
        chars
        prefix
    )

encodingPrefixC =
  getnode
    ( paltv
        [ Utf8 <$ pstr "u8",
          Char16T <$ pstr "u",
          Char32T <$ pstr "U",
          WcharT <$ pstr "L"
        ]
    )

sCharSequenceC = getnode $ pkleene sCharC

sCharC = pfn (\c -> c /= '"' && c /= '\\' && c /= '\n')
