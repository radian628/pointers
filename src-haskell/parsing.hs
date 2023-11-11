{-# LANGUAGE LambdaCase #-}

module Parsing where

import Data.Char (isAlpha, isDigit, isHexDigit)
import Data.Foldable (toList)
import Data.Function (fix)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Maybe (fromMaybe)

matchN :: (Char -> Bool) -> [Char] -> ([Char], [Char])
matchN _ [] = ([], [])
matchN pred (x : xs) =
  if pred x
    then
      let (first, rest) = matchN pred xs
       in (x : first, rest)
    else ([], x : xs)

setStartEnd parser = do
  ns <- nodeStart <$> (getnodeSkipErr $ identParser ())
  d <- parser
  ne <- nodeEnd <$> (getnodeSkipErr $ identParser ())
  noopParser $ cstexpr d ns ne

modifyStart ppnew node = case node of
  CSTExpression d pp pp' _ _ -> cstexpr d ppnew pp'
  CSTError err pp pp' _ _ -> csterror err ppnew pp'

modifySkip node skipstart skipend = case node of
  CSTExpression d pp pp' ss se ->
    CSTExpression d pp pp' (skipstart ++ ss) (se ++ skipend)
  CSTError err pp pp' ss se ->
    CSTError err pp pp' (skipstart ++ ss) (se ++ skipend)

newtype Parser a = Parser
  { parse :: ParserPointer -> CSTNode a
  }

data SkipTokenC
  = SkipTokenWhitespaceC [Char]
  | SkipTokenLineCommentC [Char]
  | SkipTokenBlockCommentC [Char]

doskip p = do
  skipstart <- pkleene skipC
  d <- p
  skipend <- pkleene skipC
  pure (modifySkip d skipstart skipend)

skipC =
  getnode $
    paltv
      [ SkipTokenWhitespaceC <$> pstr " ",
        SkipTokenWhitespaceC <$> pstr "\t",
        SkipTokenWhitespaceC <$> pstr "\r",
        SkipTokenWhitespaceC <$> pstr "\n",
        do
          pstr "//"
          SkipTokenLineCommentC <$> pkleene (pfn (/= '\n'))
          -- TODO: proper multi-line comments
          -- do
          --   pstr "/*"
      ]

mergeskip outer inner =
  modifySkip inner (skipStart outer) (skipEnd outer)

data CSTNode a
  = --- Normal value representing a successful computation.
    CSTExpression
      { nodeData :: a,
        nodeStart :: ParserPointer,
        nodeEnd :: ParserPointer,
        skipStart :: [CSTNode SkipTokenC],
        skipEnd :: [CSTNode SkipTokenC]
      }
  | CSTError
      { nodeErr :: [Char],
        nodeStart :: ParserPointer,
        nodeEnd :: ParserPointer,
        skipStart :: [CSTNode SkipTokenC],
        skipEnd :: [CSTNode SkipTokenC]
      }

cstexpr d start end = CSTExpression d start end [] []

csterror err start end = CSTError err start end [] []

instance Show a => Show (CSTNode a) where
  show = \n ->
    case n of
      CSTExpression a pp pp' _ _ -> show a
      CSTError err pp pp' _ _ -> show err

noerr defaultValue p =
  Parser
    { parse = \pp -> case parse p pp of
        CSTExpression v pp pp' _ _ ->
          cstexpr v pp pp'
        CSTError _ pp pp' _ _ ->
          cstexpr defaultValue pp pp'
    }

seterr err parser =
  Parser
    { parse = \pp -> case parse parser pp of
        CSTExpression v pp pp' _ _ ->
          cstexpr v pp pp'
        CSTError _ pp pp' _ _ ->
          csterror err pp pp'
    }

instance Functor (CSTNode) where
  fmap f x = case x of
    CSTExpression inner start end _ _ -> cstexpr (f inner) start end
    CSTError s p p2 _ _ -> csterror s p p2

nullpp =
  ParserPointer {next = Nothing, pos = 0, fullStr = [], sliceStr = []}

data ParserPointer = ParserPointer
  { next :: Maybe (Char, ParserPointer),
    pos :: Integer,
    fullStr :: [Char],
    sliceStr :: [Char]
  }
  deriving (Show)

identParser :: a -> Parser a
identParser val =
  Parser
    { parse = \pp -> cstexpr val pp pp
    }

noopParser :: CSTNode a -> Parser a
noopParser node =
  Parser
    { parse = \pp -> node
    }

instance Functor Parser where
  fmap f p =
    Parser
      { parse = \pp -> f <$> (parse p pp)
      }

instance Applicative Parser where
  pure p =
    Parser
      { parse = \pp -> cstexpr p pp pp
      }
  p1 <*> p2 =
    Parser
      { parse = \pp -> case parse p1 pp of
          CSTExpression f _ pp' _ _ -> case parse p2 pp' of
            CSTExpression v _ pp'' _ _ -> cstexpr (f v) pp pp''
            CSTError a b c _ _ -> csterror a b c
          CSTError a b c _ _ -> csterror a b c
      }

instance Monad Parser where
  (>>=) p f =
    Parser
      { parse = \pp ->
          case parse p pp of
            CSTExpression v _ pp' _ _ ->
              modifyStart pp (parse (f v) pp')
            CSTError a b c _ _ -> csterror a b b
      }

--- parser for a character predicate
pfn :: (Char -> Bool) -> Parser Char
pfn f =
  Parser
    { parse = \pp -> case next pp of
        Just (c', pp') ->
          if f c'
            then cstexpr c' pp pp'
            else csterror "TODO: actually good error messages" pp pp
        _ -> csterror "Unexpected end of input." pp pp
    }

getnode :: Parser a -> Parser (CSTNode a)
getnode p =
  Parser
    { parse = \pp ->
        let v = parse p pp
         in case v of
              CSTError a pp pp' _ _ -> csterror a pp pp'
              CSTExpression _ pp pp' _ _ ->
                cstexpr v pp pp'
    }

getnodeSkipErr :: Parser a -> Parser (CSTNode a)
getnodeSkipErr p =
  Parser
    { parse = \pp ->
        let v = parse p pp
         in case v of
              CSTError a pp pp' _ _ -> cstexpr v pp pp'
              CSTExpression _ pp pp' _ _ ->
                cstexpr v pp pp'
    }

--- parser for a single char
pchar :: Char -> Parser Char
pchar c = pfn ((==) c)

--- make a parser optional
popt :: Parser a -> Parser (Maybe a)
popt parser =
  Parser
    { parse = \pp -> case parse parser pp of
        CSTExpression v pp pp' _ _ -> cstexpr (Just v) pp pp'
        CSTError err pp pp' _ _ -> cstexpr Nothing pp pp
    }

--- parser for a string literal
pstr :: [Char] -> Parser [Char]
pstr [] = identParser ""
pstr (x : xs) =
  (uncurry (:)) <$> (pconcat (pchar x) (pstr xs))

--- parser for alternation
palt :: Parser a -> Parser a -> Parser a
palt p1 p2 =
  Parser
    { parse = \pp ->
        case parse p1 pp of
          CSTExpression v pp pp' _ _ -> cstexpr v pp pp'
          CSTError err pp pp' _ _ -> parse p2 pp
    }

--- parser for concatenation
pconcat :: Parser a -> Parser b -> Parser (a, b)
pconcat pa pb = do
  a <- pa
  b <- pb
  pure (a, b)

--- kleene star operator
pkleene p =
  Parser
    { parse =
        fix
          --- current parse operation
          ( \r pp -> case parse p pp of
              --- success -> combine it with another attempt
              CSTExpression result _ pp' _ _ ->
                let nextmatchExpr = r pp'
                 in case nextmatchExpr of
                      CSTExpression nextmatch _ pp'' _ _ ->
                        cstexpr (result : nextmatch) pp' pp''
              --- failure -> return empty list
              CSTError err pp pp' _ _ -> cstexpr [] pp pp
          )
    }

maperr :: a -> CSTNode a -> CSTNode a
maperr value node = case node of
  CSTExpression item start end _ _ -> cstexpr item start end
  CSTError msg start end _ _ -> cstexpr value start end

poneormore :: Parser a -> Parser (NonEmpty a)
poneormore p = poneormoreDifferent p p

poneormoreDifferent :: Parser a -> Parser a -> Parser (NonEmpty a)
poneormoreDifferent phead ptail = do
  head <- phead
  tail <- pkleene ptail
  pure $ head :| tail

-- parse a specific number of something
pcount :: Parser a -> Integer -> Parser [a]
pcount p 0 = pure []
pcount p n | n > 0 = do
  first <- p
  rest <- pcount p (n - 1)
  pure (first : rest)

-- parse between a lower and an upper bound of something
prange :: Parser a -> Integer -> Integer -> Parser [a]
prange p lo hi =
  pconcatv
    [ -- required lo elements
      pcount p lo,
      -- optional hi-lo elements
      fmap concat (pcount (fmap toList (popt p)) (hi - lo))
    ]
    (++)

patleast :: Parser a -> Integer -> Parser [a]
patleast p lo =
  pconcatv
    [ pcount p lo,
      pkleene p
    ]
    (++)

paltv :: [Parser a] -> Parser a
paltv (x : xs) = foldl palt x xs

pconcatv :: [Parser b] -> (b -> b -> b) -> Parser b
pconcatv xs combiner =
  foldl1 combiner <$> sequence xs -- foldl (\p1 p2 -> fmap (uncurry combiner) (pconcat p1 p2)) x xs

--- skippable whitespace tokens
pskip =
  paltv (map pchar " \n\r\t")

makeppPos str pos fullStr =
  ParserPointer
    { next =
        if not (null str)
          then Just (head str, makeppPos (tail str) (pos + 1) fullStr)
          else Nothing,
      pos = 0,
      fullStr = fullStr,
      sliceStr = str
    }

makepp str = makeppPos str 0 str

showDetail node =
  (show node)
    ++ " remainingstr:                                  "
    ++ (sliceStr (nodeEnd node))

parseWith str parser =
  let pp = (makepp str)
   in parse parser pp
