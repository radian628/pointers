{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}

module Parsing where

import Data.Char (isAlpha, isDigit, isHexDigit)
import Data.Foldable (toList)
import Data.Function (fix)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Maybe (fromMaybe)
import Language.Haskell.TH.Syntax (Lift)

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
  CSTExpression d pp pp' -> cstexpr d ppnew pp'
  CSTError err pp pp' -> csterror err ppnew pp'

newtype Parser i o = Parser
  { parse :: ParserPointer i -> ParseNode i o
  }

instance Show (Parser i o) where
  show (!x) = "$PARSER"

data SkipTokenC
  = SkipTokenWhitespaceC [Char]
  | SkipTokenLineCommentC [Char]
  | SkipTokenBlockCommentC [Char]
  deriving (Show, Eq)

-- doskip p skipTokens = do
--   skipstart <- pkleene skipC
--   d <- p
--   skipend <- pkleene skipC
--   pure (modifySkip d skipstart skipend)

skipC =
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

data ParseNode i o
  = --- Normal value representing a successful computation.
    CSTExpression
      { nodeData :: o,
        nodeStart :: ParserPointer i,
        nodeEnd :: ParserPointer i
      }
  | CSTError
      { nodeErr :: [Char],
        nodeStart :: ParserPointer i,
        nodeEnd :: ParserPointer i
      }
  deriving (Eq)

cstexpr d start end = CSTExpression d start end

csterror err start end = CSTError err start end

instance Show b => Show (ParseNode a b) where
  show = \n ->
    case n of
      CSTExpression a pp pp' -> "(cstexpr " ++ show a ++ ")"
      CSTError err pp pp' -> "(csterr " ++ show err ++ ")"

noerr defaultValue p =
  Parser
    { parse = \pp -> case parse p pp of
        CSTExpression v pp pp' ->
          cstexpr v pp pp'
        CSTError _ pp pp' ->
          cstexpr defaultValue pp pp'
    }

seterr err parser =
  Parser
    { parse = \pp -> case parse parser pp of
        CSTExpression v pp pp' ->
          cstexpr v pp pp'
        CSTError _ pp pp' ->
          csterror err pp pp'
    }

instance Functor (ParseNode a) where
  fmap f x = case x of
    CSTExpression inner start end -> cstexpr (f inner) start end
    CSTError s p p2 -> csterror s p p2

nullpp =
  ParserPointer {next = Nothing, pos = 0, fullStr = [], sliceStr = []}

data ParserPointer a = ParserPointer
  { next :: Maybe (a, ParserPointer a),
    pos :: Integer,
    fullStr :: [a],
    sliceStr :: [a]
  }
  deriving (Eq)

instance (Show a) => Show (ParserPointer a) where
  show pp = "pp:" ++ show (sliceStr pp)

identParser :: o -> Parser i o
identParser val =
  Parser
    { parse = \pp -> cstexpr val pp pp
    }

noopParser :: ParseNode i o -> Parser i o
noopParser node =
  Parser
    { parse = \pp -> node
    }

instance Functor (Parser i) where
  fmap f p =
    Parser
      { parse = \pp -> f <$> (parse p pp)
      }

instance Applicative (Parser i) where
  pure p =
    Parser
      { parse = \pp -> cstexpr p pp pp
      }
  p1 <*> p2 =
    Parser
      { parse = \pp -> case parse p1 pp of
          CSTExpression f _ pp' -> case parse p2 pp' of
            CSTExpression v _ pp'' -> cstexpr (f v) pp pp''
            CSTError a b c -> csterror a b c
          CSTError a b c -> csterror a b c
      }

instance Monad (Parser i) where
  (>>=) p f =
    Parser
      { parse = \pp ->
          case parse p pp of
            CSTExpression v _ pp' ->
              modifyStart pp (parse (f v) pp')
            CSTError a b c -> csterror a b b
      }

--- parser for any predicate for the next char/token
pfn :: (i -> Bool) -> Parser i i
pfn f =
  Parser
    { parse = \pp -> case next pp of
        Just (c', pp') ->
          if f c'
            then cstexpr c' pp pp'
            else csterror "TODO: actually good error messages" pp pp
        _ -> csterror "Unexpected end of input." pp pp
    }

pfnAndModify :: (i -> Maybe o) -> Parser i o
pfnAndModify f =
  Parser
    { parse = \pp -> case next pp of
        Just (c', pp') ->
          case f c' of
            Just o -> cstexpr o pp pp'
            Nothing -> csterror "TODO: actually good error messages" pp pp
        _ -> csterror "Unexpected end of input." pp pp
    }

unwrapTokenAndModify :: (b -> Maybe c) -> Parser (ParseNode a b) c
unwrapTokenAndModify f =
  Parser
    { parse = \pp -> case next pp of
        Just (c', pp') ->
          case c' of
            CSTExpression v _ _ ->
              case f v of
                Just o -> cstexpr o pp pp'
                Nothing -> csterror "TODO: actually good error messages" pp pp
            _ -> csterror "Unexpected end of input." pp pp
        _ -> csterror "Invalid token." pp pp
    }

getnode :: Parser i o -> Parser i (ParseNode i o)
getnode p =
  Parser
    { parse = \pp ->
        let v = parse p pp
         in case v of
              CSTError a pp pp' -> csterror a pp pp'
              CSTExpression _ pp pp' ->
                cstexpr v pp pp'
    }

getnodeSkipErr :: Parser i o -> Parser i (ParseNode i o)
getnodeSkipErr p =
  Parser
    { parse = \pp ->
        let v = parse p pp
         in case v of
              CSTError a pp pp' -> cstexpr v pp pp'
              CSTExpression _ pp pp' ->
                cstexpr v pp pp'
    }

--- parser for a single char
pchar :: Char -> Parser Char Char
pchar c = pfn ((==) c)

--- make a parser optional
popt :: Parser i o -> Parser i (Maybe o)
popt parser =
  Parser
    { parse = \pp -> case parse parser pp of
        CSTExpression v pp pp' -> cstexpr (Just v) pp pp'
        CSTError err pp pp' -> cstexpr Nothing pp pp
    }

--- parser for a string literal
pstr :: [Char] -> Parser Char [Char]
pstr [] = identParser ""
pstr (x : xs) =
  (uncurry (:)) <$> (pconcat (pchar x) (pstr xs))

--- parser for alternation
palt :: Parser i o -> Parser i o -> Parser i o
palt p1 p2 =
  Parser
    { parse = \pp ->
        case parse p1 pp of
          CSTExpression v pp pp' -> cstexpr v pp pp'
          CSTError err pp pp' -> parse p2 pp
    }

--- parser for concatenation
pconcat :: Parser i o -> Parser i o' -> Parser i (o, o')
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
              CSTExpression result _ pp' ->
                let nextmatchExpr = r pp'
                 in case nextmatchExpr of
                      CSTExpression nextmatch _ pp'' ->
                        cstexpr (result : nextmatch) pp' pp''
              --- failure -> return empty list
              CSTError err pp pp' -> cstexpr [] pp pp
          )
    }

maperr :: o -> ParseNode i o -> ParseNode i o
maperr value node = case node of
  CSTExpression item start end -> cstexpr item start end
  CSTError msg start end -> cstexpr value start end

poneormore :: Parser i o -> Parser i (NonEmpty o)
poneormore p = poneormoreDifferent p p

poneormoreDifferent :: Parser i o -> Parser i o -> Parser i (NonEmpty o)
poneormoreDifferent phead ptail = do
  head <- phead
  tail <- pkleene ptail
  pure $ head :| tail

-- parse a specific number of something
pcount :: Parser i o -> Integer -> Parser i [o]
pcount p 0 = pure []
pcount p n | n > 0 = do
  first <- p
  rest <- pcount p (n - 1)
  pure (first : rest)

-- parse between a lower and an upper bound of something
prange :: Parser i o -> Integer -> Integer -> Parser i [o]
prange p lo hi =
  pconcatv
    [ -- required lo elements
      pcount p lo,
      -- optional hi-lo elements
      fmap concat (pcount (fmap toList (popt p)) (hi - lo))
    ]
    (++)

patleast :: Parser i o -> Integer -> Parser i [o]
patleast p lo =
  pconcatv
    [ pcount p lo,
      pkleene p
    ]
    (++)

paltv :: [Parser i o] -> Parser i o
paltv (x : xs) = foldl palt x xs

pconcatv :: [Parser i o] -> (o -> o -> o) -> Parser i o
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

useInnerNode :: Parser i (ParseNode i o) -> Parser i o
useInnerNode p =
  Parser
    { parse = \pp ->
        case parse p pp of
          CSTExpression v _ _ -> v
          CSTError err start end ->
            CSTError err start end
    }
