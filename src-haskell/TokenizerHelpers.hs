{-# LANGUAGE TemplateHaskell #-}

module TokenizerHelpers where

import GrammarTypes (Token (TokenPunctuator))
import Language.Haskell.TH
import Language.Haskell.TH.Syntax (lift)
import Tokenizer

punct :: String -> Q Exp
punct str = do
  punctExpr <- (lift $ strToPunct str)
  pure $
    ( AppE
        (VarE (mkName "punctuatorT"))
        punctExpr
    )