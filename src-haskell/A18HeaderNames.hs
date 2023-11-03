module A18HeaderNames where

import Parsing

--- A.1.8 Header Names

headerNameC =
  paltv
    [ do
        pchar '<'
        chars <- hCharSequenceC
        pchar '>'
        pure chars,
      do
        pchar '"'
        chars <- qCharSequenceC
        pchar '"'
        pure chars
    ]

hCharSequenceC = pkleene qCharC

hCharC = pfn (\c -> c /= '\n' && c /= '>')

qCharSequenceC = pkleene qCharC

qCharC = pfn (\c -> c /= '\n' && c /= '"')