module A13A14A15 where

import Data.Char
import Data.Foldable
import Data.Maybe
import GrammarTypes
import GrammarUtils
import Parsing

--- A.1.3 Identifiers

identifierC = do
  init <- identifierNondigitC
  rest <- pkleene (paltv [digitC, identifierNondigitC])
  pure (init : rest)

identifierNondigitC =
  paltv
    [ nonDigitC,
      universalCharacterNameC
    ]

inBetween x min max = x >= min && x <= max

nonDigitC = pfn (\c -> inBetween (ord c) 65 90 || inBetween (ord c) 97 122 || c == '_')

digitC :: StrParser Char
digitC = pfn isDigit

--- A.1.4 Universal Character Name

universalCharacterNameC =
  paltv
    [ do
        pstr "\\u"
        q1 <- hexQuadC
        pure '0', --- TODO: actually make this work properly,
      do
        pstr "\\U"
        q1 <- hexQuadC
        q2 <- hexQuadC
        pure '0' --- TODO: actually make this work properly
    ]

hexQuadC = do
  d1 <- hexadecimalDigitC
  d2 <- hexadecimalDigitC
  d3 <- hexadecimalDigitC
  d4 <- hexadecimalDigitC
  pure '0' --- TODO: actually make this work properly

--- A.1.5 Constants

constantC =
  paltv
    [ ConstantIntCD <$> integerConstantC,
      ConstantFloatCD <$> floatingConstantC
    ]

integerConstantC = getnode $ do
  constant <-
    (seterr "constant failed") $
      paltv
        [ (seterr "hex ") hexadecimalConstantC,
          (seterr "oct ") octalConstantC,
          (seterr "dec ") decimalConstantC
        ]
  suffix <- (popt integerSuffixC)
  pure
    ( IntLiteralC
        constant
        suffix
    )

decimalConstantC :: StrParser (TokenNode Integer)
decimalConstantC =
  getnode $
    fmap read (pconcatv [fmap (: []) nonzeroDigitC, pkleene digitC] (++))

octalConstantC :: StrParser (TokenNode Integer)
octalConstantC =
  getnode $
    fmap read (pconcatv [pstr "0", pkleene octalDigitC] (++))

hexadecimalConstantC :: StrParser (TokenNode Integer)
hexadecimalConstantC =
  getnode $
    fmap read (pconcatv [hexadecimalPrefixC, pkleene hexadecimalDigitC] (++))

hexadecimalPrefixC = paltv [pstr "0x", pstr "0X"]

nonzeroDigitC = pfn (\c -> inBetween (ord c) 48 57)

octalDigitC = pfn (\c -> inBetween (ord c) 48 55)

hexadecimalDigitC =
  pfn (\c -> inBetween (ord c) 48 57 || inBetween (ord c) 97 102 || inBetween (ord c) 65 70)

integerSuffixC =
  paltv
    [ getnode $ do
        precisionSuffix <- paltv [longLongSuffixC, longSuffixC]
        signSuffix <- popt unsignedSuffixC
        pure (precisionSuffix, isJust signSuffix),
      getnode $ do
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

floatingConstantC =
  paltv
    [ decimalFloatingConstantC,
      hexadecimalFloatingConstantC
    ]

decimalFloatingConstantC = getnode $ do
  dbl <-
    fmap
      (read :: [Char] -> Double)
      ( paltv
          [ pconcatv
              [ fractionalConstantC,
                toList <$> popt exponentPartC
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
    ( FloatExprC
        dbl
        (fromMaybe Double suffix)
    )

hexadecimalFloatingConstantC =
  getnode $
    paltv
      [ do
          hexadecimalPrefixC
          mantissa <- hexadecimalFractionalConstantC
          exponent <- binaryExponentPartC
          suffix <- popt floatingSuffixC
          pure
            ( FloatExprC
                (mantissa * (2.0 ** exponent))
                (fromMaybe Double suffix)
            ),
        do
          hexadecimalPrefixC
          (mantissa :: Double) <- fmap (read . (++) "0x") hexadecimalDigitSequenceC
          exponent <- binaryExponentPartC
          suffix <- popt floatingSuffixC
          pure
            ( FloatExprC
                (mantissa * (2.0 ** exponent))
                (fromMaybe Double suffix)
            )
      ]

fractionalConstantC :: StrParser [Char]
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

binaryExponentPartC :: StrParser Double
binaryExponentPartC = do
  palt (pchar 'p') (pchar 'P')
  fmap (read . stripLeadingPlus) (pconcatv [toList <$> popt signC, digitSequenceC] (++))

hexadecimalDigitSequenceC =
  pconcatv [fmap (: []) hexadecimalDigitC, pkleene hexadecimalDigitC] (++)

floatingSuffixC :: StrParser FloatPrecision
floatingSuffixC =
  paltv
    [ fmap (const Float) (paltv (map (fmap (: []) . pchar) "fF")),
      fmap (const Float) (paltv (map (fmap (: []) . pchar) "lL"))
    ]

enumerationConstantC = identifierC

characterConstantC = do
  prefix <-
    paltv
      ( map
          (\(code, str) -> code <$ pstr str)
          [ (CharEncodeTypeL, "L"),
            (CharEncodeTypeSmallU, "u"),
            (CharEncodeTypeBigU, "U")
          ]
      )
  pchar '\''
  seq <- cCharSequenceC
  pchar '\''
  pure (prefix, seq)

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

octalEscapeSequenceC :: StrParser Char
octalEscapeSequenceC = do
  pchar '\\'
  fmap (chr . read . (++) "0o") (prange octalDigitC 1 3)

hexadecimalEscapeSequenceC = do
  pstr "\\x"
  fmap (chr . read . (++) "0x") (pkleene hexadecimalDigitC)