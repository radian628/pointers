module A19PreprocessingNumbers where

import A13Identifiers
import A15Constants
import Parsing

--- A.1.9 Preprocessing Numbers

ppNumberC = do
  --- start with optional dot
  popt (pchar '.')
  --- digits
  digits <- pkleene digitC
  --- letters and signs and stuff idk
  lettersAndSigns <-
    pkleene
      ( paltv
          [ do
              pchar 'e'
              signC,
            do
              pchar 'E'
              signC,
            do
              pchar 'p'
              signC,
            do
              pchar 'P'
              signC,
            identifierNondigitC,
            pchar '.'
          ]
      )
  pure (digits, lettersAndSigns)