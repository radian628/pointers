module A18HeaderNames where

import GrammarTypes (Token (TokenBracketedHeaderName, TokenQuotedHeaderName))
import Parsing

--- A.1.8 Header Names

headerNameC =
  paltv
    [ do
        pchar '<'
        chars <- TokenBracketedHeaderName <$> hCharSequenceC
        pchar '>'
        pure chars,
      do
        pchar '"'
        chars <- TokenQuotedHeaderName <$> qCharSequenceC
        pchar '"'
        pure chars
    ]

hCharSequenceC = pkleene qCharC

hCharC = pfn (\c -> c /= '\n' && c /= '>')

qCharSequenceC = pkleene qCharC

qCharC = pfn (\c -> c /= '\n' && c /= '"')