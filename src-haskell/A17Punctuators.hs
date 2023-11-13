module A17Punctuators where

import GrammarTypes
import Parsing

--- A.1.7 Punctuators

punctuatorC =
  paltv
    ( map
        (\(str, p) -> p <$ pstr str)
        [ ("[", PSquareOpen),
          ("]", PSquareClosed),
          ("{", PCurlyOpen),
          ("}", PCurlyClosed),
          ("(", PParenOpen),
          (")", PParenClosed),
          (".", PDot),
          ("->", PArrow),
          ("++", PInc),
          ("--", PDec),
          ("&", PAmpersand),
          ("*", PStar),
          ("+", PPlus),
          ("-", PMinus),
          ("~", PTilde),
          ("!", PExclamation),
          ("/", PSlash),
          ("%", PPercent),
          ("<<", PShiftLeft),
          (">>", PShiftRight),
          ("<", PLessThan),
          (">", PGreaterThan),
          ("<=", PLessEqual),
          (">=", PGreaterEqual),
          ("==", PEqual),
          ("!=", PNotEqual),
          ("&&", PLogicalAnd),
          ("||", PLogicalOr),
          ("^", PXor),
          ("|", POr),
          ("?", PQuestionMark),
          (":", PColon),
          (";", PSemicolon),
          ("...", PEllipsis),
          ("=", PAssignment),
          ("*=", PTimesEquals),
          ("/=", PDivEquals),
          ("%=", PModEquals),
          ("+=", PAddEquals),
          ("-=", PSubEquals),
          ("<<=", PLeftShiftEquals),
          (">>=", PRightShiftEquals),
          ("&=", PAndEquals),
          ("^=", PXorEquals),
          ("|=", POrEquals),
          (",", PComma),
          ("#", PHash),
          ("##", PDoubleHash),
          ("<:", PSquareOpen),
          (":>", PSquareClosed),
          ("<%", PCurlyOpen),
          ("%>", PCurlyClosed),
          ("%:", PHash),
          ("%:%:", PDoubleHash)
        ]
    )
