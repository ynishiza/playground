{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use infix" #-}
{- ORMOLU_DISABLE -}
module Combinators
  ( 
    -- Primitive
    (+++),
    (<++),
    gather,

    -- Other
    pfail,
    eof,
    satisfy,
    char,
    string,
    munch,
    munch1,
    choice,
    skipSpaces,
    between,
    count,
    option,
    optional,
    many,
    many1,
    skipMany,
    skipMany1,

    module X,
  )
where
{- ORMOLU_ENABLE -}

import Base.Parse qualified as PB
import Control.Applicative as X hiding (many, optional, some)
import Control.Monad
import Data.List (isPrefixOf)
import P as X

(+++) :: P a -> P a -> P a
(+++) = (<|>)

(<++) :: P a -> P a -> P a
(ParseBuilder p) <++ (ParseBuilder q) = ParseBuilder $ \k ->
  let left = p k
   in case left of
        (PB.Result _ _) -> left
        (PB.Final _) -> left
        _ -> q k

gather :: forall a. P a -> P (String, a)
gather p = do
  str <- look
  let res = parse p str
      mp :: (a, String) -> ((String, a), String)
      mp (a, s) = ((take (length str - length s) str, a), s)
  parserToP $ PB.final $ mp <$> res

pfail :: P a
pfail = stop

eof :: P ()
eof = do
  str <- look
  unless (null str) pfail

satisfy :: (Char -> Bool) -> P Char
satisfy predicate = do
  c <- get
  if predicate c then return c else pfail

char :: Char -> P Char
char c = satisfy (== c)

string :: String -> P String
string toMatch = do
  str <- look
  if isPrefixOf toMatch str then count (length toMatch) get else pfail

munch :: (Char -> Bool) -> P String
munch predicate = do
  str <- look
  count (length $ takeWhile predicate str) get

munch1 :: (Char -> Bool) -> P String
munch1 predicate = do
  str <- munch predicate
  if null str then pfail else return str

skipSpaces :: P ()
skipSpaces = skipManyWhile (`elem` " \n\t\r")

choice :: [P a] -> P a
choice = msum

count :: Int -> P a -> P [a]
count n p
  | n > 0 = (:) <$> p <*> count (n - 1) p
  | n == 0 = return []
  | otherwise = pfail

between :: P start -> P end -> P a -> P a
between start end p = do
  _ <- start
  res <- p
  _ <- end
  return res

option :: a -> P a -> P a
option a p = p <|> return a

optional :: P a -> P ()
optional p = void p <|> return ()

many :: P a -> P [a]
many p = return [] <|> many1 p

many1 :: P a -> P [a]
many1 p = (:) <$> p <*> many p

skipMany :: P a -> P ()
skipMany p = void $ many p

skipMany1 :: P a -> P ()
skipMany1 p = p >> skipMany p

skipManyWhile :: (Char -> Bool) -> P ()
skipManyWhile predicate = do
  str <- look
  loop str
  where
    loop (c : cs) = when (predicate c) $ get >> loop cs
    loop _ = pure ()
