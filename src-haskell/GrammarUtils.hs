module GrammarUtils where

import Data.Char (chr, isAlpha, isDigit, ord)
import Data.Foldable (toList)
import Data.Maybe (fromMaybe, isJust)
import Data.Text.Lazy.Builder.Int (hexadecimal)
import Data.Void (Void)
import GHC.Real ((%))
import GrammarTypes
import Parsing