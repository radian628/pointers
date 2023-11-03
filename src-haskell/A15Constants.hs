module A15Constants where

import A13Identifiers
import A14UniversalCharacterNames
import Data.Char
import Data.Foldable
import Data.Maybe
import GrammarTypes
import Parsing

--- A.1.5 Constants

constantC =
  paltv
    [ integerConstantC
    ]

integerConstantC :: Parser CSTExpressionData
integerConstantC = do
  constant <-
    paltv
      [ hexadecimalConstantC,
        decimalConstantC,
        octalConstantC
      ]
  suffix <- fmap (fromMaybe (Int, True)) (popt integerSuffixC)
  pure
    IntExpr
      { precision = fst suffix,
        value = constant,
        unsigned = snd suffix
      }

decimalConstantC :: Parser Integer
decimalConstantC =
  fmap read (pconcatv [fmap (: []) nonzeroDigitC, pkleene digitC] (++))

octalConstantC :: Parser Integer
octalConstantC =
  fmap read (pconcatv [pstr "0", pkleene octalDigitC] (++))

hexadecimalConstantC :: Parser Integer
hexadecimalConstantC =
  fmap read (pconcatv [hexadecimalPrefixC, pkleene hexadecimalDigitC] (++))

hexadecimalPrefixC = paltv [pstr "0x", pstr "0X"]

nonzeroDigitC = pfn (\c -> inBetween (ord c) 48 57)

octalDigitC = pfn (\c -> inBetween (ord c) 48 55)

hexadecimalDigitC =
  pfn (\c -> inBetween (ord c) 48 57 || inBetween (ord c) 97 102 || inBetween (ord c) 65 70)

integerSuffixC :: Parser (Precision, Bool)
integerSuffixC =
  paltv
    [ do
        precisionSuffix <- paltv [longLongSuffixC, longSuffixC]
        signSuffix <- popt unsignedSuffixC
        pure (precisionSuffix, isJust signSuffix),
      do
        signSuffix <- unsignedSuffixC
        precisionSuffix <- popt (paltv [longLongSuffixC, longSuffixC])
        pure (fromMaybe Int precisionSuffix, True)
    ]

unsignedSuffixC = do
  paltv [pstr "u", pstr "U"]
  pure ()

longSuffixC = do
  paltv [pstr "l", pstr "L"]
  pure Long

longLongSuffixC = do
  paltv [pstr "ll", pstr "LL"]
  pure LongLong

floatingConstant =
  paltv
    [ decimalFloatingConstantC,
      hexadecimalFloatingConstantC
    ]

decimalFloatingConstantC = do
  dbl <-
    fmap
      (read :: [Char] -> Double)
      ( paltv
          [ pconcatv
              [ fractionalConstantC,
                poptChar exponentPartC
              ]
              (++),
            pconcatv
              [ digitSequenceC,
                fmap (: []) exponentPartC
              ]
              (++)
          ]
      )
  suffix <- popt floatingSuffixC
  pure
    FloatExpr
      { float = dbl,
        floatPrecision = fromMaybe Double suffix
      }

hexadecimalFloatingConstantC :: Parser CSTExpressionData
hexadecimalFloatingConstantC =
  paltv
    [ do
        hexadecimalPrefixC
        mantissa <- hexadecimalFractionalConstantC
        exponent <- binaryExponentPartC
        suffix <- popt floatingSuffixC
        pure
          FloatExpr
            { float = mantissa * (2.0 ** exponent),
              floatPrecision = fromMaybe Double suffix
            },
      do
        hexadecimalPrefixC
        (mantissa :: Double) <- fmap (read . (++) "0x") hexadecimalDigitSequenceC
        exponent <- binaryExponentPartC
        suffix <- popt floatingSuffixC
        pure
          FloatExpr
            { float = mantissa * (2.0 ** exponent),
              floatPrecision = fromMaybe Double suffix
            }
    ]

fractionalConstantC :: Parser [Char]
fractionalConstantC =
  paltv
    [ pconcatv [fmap (concat . toList) (popt digitSequenceC), pstr ".", digitSequenceC] (++),
      pconcatv [digitSequenceC, pstr "."] (++)
    ]

exponentPartC = paltv [pchar 'e', pchar 'E']

signC = paltv [pchar '+', pchar '-']

digitSequenceC = pconcatv [fmap (: []) digitC, pkleene digitC] (++)

hexadecimalFractionalConstantC =
  paltv
    [ do
        (value :: Double) <- fmap ((read . (++) "0x") . fromMaybe "0") (popt hexadecimalDigitSequenceC)
        pstr "."
        (fractional :: Double) <- fmap (read . (++) "0x") hexadecimalDigitSequenceC
        pure
          ( value
              + fractional
                * (1 / 16) ** fromIntegral (1 + floor (logBase 16.0 fractional) :: Integer)
          ),
      do
        (value :: Double) <- fmap (read . (++) "0x") hexadecimalDigitSequenceC
        pstr "."
        pure value
    ]

stripLeadingPlus (x : xs) = if x == '+' then xs else x : xs

binaryExponentPartC :: Parser Double
binaryExponentPartC = do
  palt (pchar 'p') (pchar 'P')
  fmap (read . stripLeadingPlus) (pconcatv [poptChar signC, digitSequenceC] (++))

hexadecimalDigitSequenceC =
  pconcatv [fmap (: []) hexadecimalDigitC, pkleene hexadecimalDigitC] (++)

floatingSuffixC :: Parser FloatPrecision
floatingSuffixC =
  paltv
    [ fmap (const Float) (paltv (map (fmap (: []) . pchar) "fF")),
      fmap (const Float) (paltv (map (fmap (: []) . pchar) "lL"))
    ]

enumerationConstantC = identifierC

characterConstantC = do
  prefix <- paltv (map pchar "LuU")
  pchar '\''
  seq <- cCharSequenceC
  pchar '\''
  pure (seq, prefix)

cCharSequenceC = pkleene cCharC

cCharC =
  pfn (\c -> c /= '\'' && c /= '\\' && c /= '\n')

escapeSequenceC =
  paltv
    [ universalCharacterNameC,
      simpleEscapeSequenceC,
      octalEscapeSequenceC,
      hexadecimalEscapeSequenceC
    ]

simpleEscapeSequenceC = do
  pchar '\\'
  paltv
    [ '\'' <$ pchar '\'',
      '\"' <$ pchar '\"',
      '?' <$ pchar '?',
      '\\' <$ pchar '\\',
      '\a' <$ pchar 'a',
      '\b' <$ pchar 'b',
      '\f' <$ pchar 'f',
      '\n' <$ pchar 'n',
      '\r' <$ pchar 'r',
      '\t' <$ pchar 't',
      '\v' <$ pchar 'v'
    ]

octalEscapeSequenceC :: Parser Char
octalEscapeSequenceC = do
  pchar '\\'
  fmap (chr . read . (++) "0o") (prange octalDigitC 1 3)

hexadecimalEscapeSequenceC = do
  pstr "\\x"
  fmap (chr . read . (++) "0x") (pkleene hexadecimalDigitC)