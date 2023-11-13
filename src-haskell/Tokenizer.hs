{-# LANGUAGE TemplateHaskell #-}

module Tokenizer where

import A12Keywords
import A13A14A15
import A16StringLiterals (stringLiteralC)
import A17Punctuators (punctuatorC)
import A18HeaderNames
import GrammarTypes
import Language.Haskell.TH
import Language.Haskell.TH.Syntax (lift)
import Parsing

parseTokenC =
  getnode $
    paltv
      [ TokenKeyword
          <$> keywordC,
        TokenIdentifier
          <$> identifierC,
        TokenConstant
          <$> constantC,
        uncurry TokenCharacterConstant
          <$> characterConstantC,
        TokenStringLiteral
          <$> stringLiteralC,
        TokenPunctuator <$> punctuatorC,
        TokenSkip <$> skipC,
        headerNameC
      ]

parseTokenListC = pkleene parseTokenC

eqToToken x = getnode $ pfn (== x)

doskip p = do
  pkleene skipT
  v <- p
  pkleene skipT
  pure v

kw x = doskip $ getnode $ unwrapTokenAndModify $ \i ->
  case i of
    TokenKeyword kw -> if x == kw then Just x else Nothing
    _ -> Nothing

identifierT = doskip $ getnode $ unwrapTokenAndModify $ \i ->
  case i of
    TokenIdentifier str -> Just str
    _ -> Nothing

constantT = doskip $
  getnode $
    unwrapTokenAndModify $
      \t -> case t of
        TokenConstant x -> Just x
        _ -> Nothing

charConstantT = doskip $
  unwrapTokenAndModify $
    \t -> case t of
      TokenCharacterConstant a b -> Just (a, b)
      _ -> Nothing

stringLiteralT = doskip $
  unwrapTokenAndModify $
    \t -> case t of
      TokenStringLiteral str -> Just str
      _ -> Nothing

punctuatorT x = doskip $ getnode $ unwrapTokenAndModify $ \i ->
  case i of
    TokenPunctuator p -> if x == p then Just x else Nothing
    _ -> Nothing

skipT = getnode $ unwrapTokenAndModify $ \i ->
  case i of
    TokenSkip x -> Just x
    _ -> Nothing

quotedHeaderNameT x = doskip $
  unwrapTokenAndModify $
    \t -> case t of
      TokenQuotedHeaderName name -> Just name
      _ -> Nothing

bracketedHeaderNameT x = doskip $
  unwrapTokenAndModify $
    \t -> case t of
      TokenBracketedHeaderName name -> Just name
      _ -> Nothing

strToPunct str = case str of
  "[" -> PSquareOpen
  "]" -> PSquareClosed
  "(" -> PParenOpen
  ")" -> PParenClosed
  "{" -> PCurlyOpen
  "}" -> PCurlyClosed
  "." -> PDot
  "->" -> PArrow
  "++" -> PInc
  "--" -> PDec
  "&" -> PAmpersand
  "*" -> PStar
  "+" -> PPlus
  "-" -> PMinus
  "~" -> PTilde
  "!" -> PExclamation
  "/" -> PSlash
  "%" -> PPercent
  "<<" -> PShiftLeft
  ">>" -> PShiftRight
  "<" -> PLessThan
  ">" -> PGreaterThan
  "<=" -> PLessEqual
  ">=" -> PGreaterEqual
  "==" -> PEqual
  "!=" -> PNotEqual
  "^" -> PXor
  "|" -> POr
  "&&" -> PLogicalAnd
  "||" -> PLogicalOr
  "?" -> PQuestionMark
  ":" -> PColon
  ";" -> PSemicolon
  "..." -> PEllipsis
  "=" -> PAssignment
  "*=" -> PTimesEquals
  "/=" -> PDivEquals
  "%=" -> PModEquals
  "+=" -> PAddEquals
  "-=" -> PSubEquals
  "<<=" -> PLeftShiftEquals
  ">>=" -> PRightShiftEquals
  "&=" -> PAndEquals
  "^=" -> PXorEquals
  "|=" -> POrEquals
  "," -> PComma
  "#" -> PHash
  "##" -> PDoubleHash
  _ -> error "unknown symbol"