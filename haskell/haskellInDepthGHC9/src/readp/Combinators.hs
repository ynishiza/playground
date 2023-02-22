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
    sepBy,
    sepBy1,
    endBy,
    endBy1,
    chainr,
    chainr1,
    chainl,
    chainl1,
    manyTill,

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
(ParseBuilder pa) <++ (ParseBuilder q) = ParseBuilder $ \k ->
  let left = pa k
   in case left of
        (PB.Result _ _) -> left
        (PB.Final _) -> left
        _ -> q k

gather :: forall a. P a -> P (String, a)
gather pa = do
  str <- look
  let res = parse pa str
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
count n pa
  | n > 0 = (:) <$> pa <*> count (n - 1) pa
  | n == 0 = return []
  | otherwise = pfail

between :: P start -> P end -> P a -> P a
between start end pa = do
  _ <- start
  res <- pa
  _ <- end
  return res

option :: a -> P a -> P a
option a pa = pa <|> return a

optional :: P a -> P ()
optional pa = void pa <|> return ()

many :: P a -> P [a]
many pa = return [] <|> many1 pa

many1 :: P a -> P [a]
many1 pa = (:) <$> pa <*> many pa

skipMany :: P a -> P ()
skipMany pa = void $ many pa

skipMany1 :: P a -> P ()
skipMany1 pa = pa >> skipMany pa

skipManyWhile :: (Char -> Bool) -> P ()
skipManyWhile predicate = do
  str <- look
  loop str
  where
    loop (c : cs) = when (predicate c) $ get >> loop cs
    loop _ = pure ()

sepBy :: P a -> P sep -> P [a]
sepBy pa psep = return [] <|> sepBy1 pa psep

sepBy1 :: forall a sep. P a -> P sep -> P [a]
sepBy1 pa psep = do
  v <- pa
  return [v] <|> ((v :) <$> loop)
  where
    loop :: P [a]
    loop = do
      _ <- psep
      v <- pa
      return [v] <|> ((v :) <$> loop)

endBy :: P a -> P sep -> P [a]
endBy pa sep = return [] <|> endBy1 pa sep

endBy1 :: forall a sep. P a -> P sep -> P [a]
endBy1 pa psep = do
  v <- pa
  _ <- psep
  return [v] <|> ((v :) <$> loop)
  where
    loop :: P [a]
    loop = do
      v <- pa
      _ <- psep
      return [v] <|> ((v :) <$> loop)

chainr :: P a -> P (a -> a -> a) -> a -> P a
chainr pa pop a = return a <|> chainr1 pa pop 

chainr1 :: P a -> P (a -> a -> a) -> P a
chainr1 pa pop = do
  v <- pa
  return v <|> chainr1With v pa pop

chainr1With :: a -> P a -> P (a -> a -> a) -> P a
chainr1With u pa pop = do
  op <- pop
  v <- pa
  return (op u v) <|> (op u <$> chainr1With v pa pop)

chainl :: P a -> P (a -> a -> a) -> a -> P a
chainl pa pop a = return a <|> chainl1 pa pop 

chainl1 :: P a -> P (a -> a -> a) -> P a
chainl1 pa pop = do
  v <- pa
  return v <|> chainl1With v pa pop

chainl1With :: a -> P a -> P (a -> a -> a) -> P a
chainl1With u pa pop = do
  op <- pop
  v <- pa
  return (op u v) <|> chainl1With (op u v) pa pop

manyTill :: P a -> P a -> P [a]
manyTill pa pend = ended <|> loop
  where
    ended = pend >> return []
    loop = do
      v <- pa
      (v:) <$> (ended <|> loop)
