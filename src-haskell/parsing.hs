{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ExistentialQuantification #-}
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
  noopParser $ CSTExpression d ns ne

newtype Parser a = Parser
  { parse :: ParserPointer -> ParserPointer -> CSTNode a
  }

data CSTNode a
  = --- Normal value representing a successful computation.
    CSTExpression
      { nodeData :: a,
        nodeStart :: ParserPointer,
        nodeEnd :: ParserPointer
      }
  | CSTError
      { nodeErr :: [Char],
        nodeStart :: ParserPointer,
        nodeEnd :: ParserPointer
      }

instance Show a => Show (CSTNode a) where
  show = \n ->
    case n of
      CSTExpression a pp pp' -> show a
      CSTError err pp pp' -> show err

noerr defaultValue p =
  Parser
    { parse = \initpp pp -> case parse p initpp pp of
        CSTExpression v pp pp' ->
          CSTExpression v pp pp'
        CSTError _ pp pp' ->
          CSTExpression defaultValue pp pp'
    }

seterr err parser =
  Parser
    { parse = \initpp pp -> case parse parser initpp pp of
        CSTExpression v pp pp' ->
          CSTExpression v pp pp'
        CSTError _ pp pp' ->
          CSTError err pp pp'
    }

instance Functor (CSTNode) where
  fmap f x = case x of
    CSTExpression inner start end -> CSTExpression (f inner) start end
    CSTError s p p2 -> CSTError s p p2

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
    { parse = \initpp pp -> CSTExpression val initpp pp
    }

noopParser :: CSTNode a -> Parser a
noopParser node =
  Parser
    { parse = \initpp pp -> node
    }

instance Functor Parser where
  fmap f p =
    Parser
      { parse = \initpp pp -> f <$> (parse p initpp pp)
      }

instance Applicative Parser where
  pure p =
    Parser
      { parse = \initpp pp -> CSTExpression p initpp pp
      }
  p1 <*> p2 =
    Parser
      { parse = \initpp pp -> case parse p1 initpp pp of
          CSTExpression f _ pp' -> case parse p2 initpp pp' of
            CSTExpression v _ pp'' -> CSTExpression (f v) pp pp''
            CSTError a b c -> CSTError a b c
          CSTError a b c -> CSTError a b c
      }

instance Monad Parser where
  (>>=) p f =
    Parser
      { parse = \initpp pp ->
          case parse p initpp pp of
            CSTExpression v _ pp' ->
              parse (f v) initpp pp'
            CSTError a b c -> CSTError a initpp c
      }

--- parser for a character predicate
pfn :: (Char -> Bool) -> Parser Char
pfn f =
  Parser
    { parse = \initpp pp -> case next pp of
        Just (c', pp') ->
          if f c'
            then CSTExpression c' pp pp'
            else CSTError "TODO: actually good error messages" pp pp
        _ -> CSTError "Unexpected end of input." pp pp
    }

getnode :: Parser a -> Parser (CSTNode a)
getnode p =
  Parser
    { parse = \initpp pp ->
        let v = parse p initpp pp
         in case v of
              CSTError a pp pp' -> CSTError a pp pp'
              CSTExpression _ pp pp' ->
                CSTExpression v pp pp'
    }

getnodeSkipErr :: Parser a -> Parser (CSTNode a)
getnodeSkipErr p =
  Parser
    { parse = \initpp pp ->
        let v = parse p initpp pp
         in case v of
              CSTError a pp pp' -> CSTExpression v pp pp'
              CSTExpression _ pp pp' ->
                CSTExpression v pp pp'
    }

--- parser for a single char
pchar :: Char -> Parser Char
pchar c = pfn ((==) c)

--- make a parser optional
popt :: Parser a -> Parser (Maybe a)
popt parser =
  Parser
    { parse = \initpp pp -> case parse parser initpp pp of
        CSTExpression v pp pp' -> CSTExpression (Just v) pp pp'
        CSTError err pp pp' -> CSTExpression Nothing pp pp
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
    { parse = \initpp pp ->
        case parse p1 initpp pp of
          CSTExpression v pp pp' -> CSTExpression v pp pp'
          CSTError err pp pp' -> parse p2 initpp pp
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
        \initpp pp ->
          fix
            --- current parse operation
            ( \r (initpp, pp) -> case parse p initpp pp of
                --- success -> combine it with another attempt
                CSTExpression result _ pp' ->
                  let nextmatchExpr = maperr [] (r (initpp, pp'))
                   in case nextmatchExpr of
                        CSTExpression nextmatch _ pp'' ->
                          CSTExpression (result : nextmatch) initpp pp''
                --- failure -> return empty list
                CSTError _ pp pp' -> CSTExpression [] initpp pp
            )
            ( initpp,
              pp
            )
    }

-- pkleeneInner :: Parser a -> ParserPointer -> ParserPointer -> Parser [a]
-- pkleeneInner p initpp pp = case parse p pp of
--   CSTExpression v _ pp' -> ((v :) <$> pkleeneInner p initpp pp')
--   CSTError err _ pp' -> pure []

-- Parser
--   { --- use fix to keep recursively matching until we can't match anymore
--     parse =
--         -- fix
--         --   ( \r str -> case parse p str of
--         --       Just (result, rest) ->
--         --         let (nextmatch, rest') = fromMaybe ([result], rest) (r rest)
--         --          in Just (result : nextmatch, rest')
--         --       Nothing ->
--         --         Just ([], str)
--         --   )
--   }

maperr :: a -> CSTNode a -> CSTNode a
maperr value node = case node of
  CSTExpression item start end -> CSTExpression item start end
  CSTError msg start end -> CSTExpression value start end

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

parseWith str parser =
  let pp = (makepp str)
   in parse parser pp pp
