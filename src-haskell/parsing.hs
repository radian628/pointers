module Parsing where

{-# LANGUAGE LambdaCase #-}
import Data.Char (isDigit, isHexDigit, isAlpha)
import Data.Function (fix)
import Data.Foldable (toList)
import Data.Maybe (fromMaybe)


matchN :: (Char -> Bool) -> [Char] -> ([Char], [Char])
matchN _ [] = ([], [])
matchN pred (x:xs) =
  if pred x then
    let (first, rest) = matchN pred xs
    in (x:first, rest)
  else 
    ([], x:xs)


newtype Parser a = Parser {
  parse :: ParserPointer -> Maybe (a, ParserPointer)
}

data ParserNext a = ParserNext a ParserPointer | Done

data ParserPointer = ParserPointer {
  next :: Maybe (Char, ParserPointer),
  pos :: Integer,
  fullStr :: [Char],
  sliceStr :: [Char]
} deriving Show

identParser val = Parser {
  parse = \ str -> Just (val, str)
}

instance Functor Parser where
  fmap f p = Parser {
    parse = \ str -> case parse p str of
      Just (a, rest) -> Just (f a, rest)
      Nothing -> Nothing
  } 

instance Applicative Parser where
  pure p = Parser { 
      parse = \ pp -> Just (p, pp)
    }
  p1 <*> p2 = Parser {
    parse = \pp -> case parse p1 pp of
      Just (f, pp') -> case parse p2 pp' of
        Just (a, pp'') -> Just (f a, pp'')
        Nothing -> Nothing
      Nothing -> Nothing
  }
    
instance Monad Parser where
  (>>=) p f = Parser {
    parse = \pp -> case parse p pp of
      Just (v, pp') -> parse (f v) pp'
      Nothing -> Nothing
  }


--- parser for a single char
pchar :: Char -> Parser Char
pchar c =
  Parser {
    parse = \pp -> case next pp of
      Just (c', pp') -> if c' == c then Just (c, pp') else Nothing
      Nothing -> Nothing
  }

--- parser for a character predicate
pfn :: (Char -> Bool) -> Parser Char
pfn f =
  Parser {
    parse = \pp -> case next pp of
      Just (c', pp') -> if f c' then Just (c', pp') else Nothing
      _ -> Nothing
  }

--- make a parser optional
popt :: Parser a -> Parser (Maybe a)
popt parser = 
  Parser {
    parse = \pp -> case parse parser pp of
      Just (v, pp') -> Just (Just v, pp') 
      Nothing -> Just (Nothing, pp)
  }


poptChar :: Parser Char -> Parser [Char]
poptChar parser =
  poptStr (fmap (: []) parser)

poptStr :: Parser [Char] -> Parser [Char]
poptStr parser = 
  fmap (concat . toList) (popt parser) 

--- parser for a string literal
pstr :: [Char] -> Parser [Char]
pstr [] = identParser ""
pstr (x:xs) = Parser {
  parse = \ str -> case (parse (pconcat (pchar x) (pstr xs))) str of
    Just (_, rest) -> Just (x:xs, rest)
    Nothing -> Nothing
}

--- parser for alternation
palt :: Parser a -> Parser a -> Parser a
palt p1 p2 =
  Parser {
    parse = \ str -> case (parse p1) str of
      Just a -> Just a
      Nothing -> case (parse p2) str of
        Just a -> Just a
        Nothing -> Nothing
  }

--- parser for concatenation
pconcat :: Parser a -> Parser b -> Parser (a, b)
pconcat pa pb =
  Parser {
    parse = \ str -> case (parse pa) str of
      Just (a, rest) -> case (parse pb) rest of 
        Just (b, rest') -> Just ((a, b), rest')
        Nothing -> Nothing
      Nothing -> Nothing
  }

--- kleene star operator
pkleene :: Parser a -> Parser [a]
pkleene p =
  Parser {
    --- use fix to keep recursively matching until we can't match anymore
    parse = fix (\r str -> case parse p str of 
      Just (result, rest) -> 
        let (nextmatch, rest') = fromMaybe ([result], rest) (r rest) in 
        Just (result : nextmatch, rest') 
      Nothing -> 
        Just ([], str))
  }

-- parse a specific number of something
pcount :: Parser a -> Integer -> Parser [a]
pcount p howMany =
  Parser {
    parse = \initpp -> fst (fix (\r (pp :: ParserPointer, count :: Integer) -> 
      if count == 0 then (Just ([], pp), 0) else
      case parse p pp of
        Just (result, restpp) ->
          case r (restpp, count - 1) of
            (Just (results, restpp), count') -> (Just (result:results, restpp), count')
            (Nothing, count') -> (Nothing, count')
        Nothing -> (Nothing, count)
        ) (initpp, howMany))
  }

-- parse between a lower and an upper bound of something
prange :: Parser a -> Integer -> Integer -> Parser [a]
prange p lo hi =
  pconcatv [
    -- required lo elements
    pcount p lo,
    -- optional hi-lo elements
    fmap concat (pcount (fmap toList (popt p)) (hi - lo))
    ] (++)

paltv (x:xs) = foldl palt x xs

pconcatv :: [Parser b] -> (b -> b -> b) -> Parser b
pconcatv (x:xs) combiner = 
  foldl (\p1 p2 -> fmap (uncurry combiner) (pconcat p1 p2)) x xs

--- skippable whitespace tokens
pskip = 
  paltv (map pchar " \n\r\t") 

makeppPos str pos fullStr =
  ParserPointer {
    next = if not (null str) then 
      Just (head str, makeppPos (tail str) (pos + 1) fullStr) 
      else Nothing,
    pos = 0,
    fullStr = fullStr,
    sliceStr = str
  }

makepp str = makeppPos str 0 str

parseWith :: [Char] -> Parser a -> Maybe (a, ParserPointer)
parseWith str parser = parse parser (makepp str)
