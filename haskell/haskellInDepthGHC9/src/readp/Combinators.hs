{-# HLINT ignore "Use infix" #-}
{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
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
    many',
    many1',
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
import Data.Functor
import Data.List (isPrefixOf)
import P as X
import Data.Function ((&))

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
pfail = empty

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

choice :: Alternative f => [f a] -> f a
choice = asum

count :: Alternative f => Int -> f a -> f [a]
count n pa
  | n > 0 = (:) <$> pa <*> count (n - 1) pa
  | n == 0 = pure []
  | otherwise = empty

between :: P start -> P end -> P a -> P a
between start end pa = do
  _ <- start
  res <- pa
  _ <- end
  return res

option :: Alternative f => a -> f a -> f a
option a pa = pa <|> pure a

optional :: Alternative f => f a -> f ()
optional pa = void pa <|> pure ()

many :: Alternative f => f a -> f [a]
many pa = pure [] <|> many1 pa

many1 :: Alternative f => f a -> f [a]
many1 pa = (:) <$> pa <*> many pa

many' :: Alternative m => m a -> m [a]
many' pa = pure [] <|> many1' pa

many1' :: Alternative m => m a -> m [a]
many1' pa = ((\(!x) -> (x :)) <$> pa) <*> many' pa

skipMany :: Alternative f => f a -> f ()
skipMany pa = void $ many pa

skipMany1 :: Alternative f => f a -> f ()
skipMany1 pa = pa *> skipMany pa

skipManyWhile :: (Char -> Bool) -> P ()
skipManyWhile predicate = look >>= loop
  where
    loop (c : cs) = when (predicate c) $ get >> loop cs
    loop _ = pure ()

sepBy :: Alternative f => f a -> f sep -> f [a]
sepBy pa psep = pure [] <|> sepBy1 pa psep

sepBy1 :: forall f a sep. Alternative f => f a -> f sep -> f [a]
sepBy1 pa psep = loop
  where
    loop :: f [a]
    loop = ((:) <$> pa) <*> (pure [] <|> (psep *> loop))

endBy :: Alternative f => f a -> f sep -> f [a]
endBy pa sep = pure [] <|> endBy1 pa sep

endBy1 :: forall f a sep. Alternative f => f a -> f sep -> f [a]
endBy1 pa psep = loop
  where
    loop :: f [a]
    loop = ((:) <$> pa) <*> (psep *> (pure [] <|> loop))

-- (a1 * (a2 * (... an-1 *(an * a0))))
chainr :: Alternative f => f a -> f (a -> a -> a) -> a -> f a
chainr pa pop a = pure a <|> chainr1 pa pop

chainr1 :: Alternative f => f a -> f (a -> a -> a) -> f a
chainr1 pa pop = pa <|> loop
  where
    loop = (&) <$> pa <*> pop <*> (pa <|> loop)

-- ((.. ((a0 * a1) * a2) * a3 ...) * an-1) * an
chainl :: Alternative f => f a -> f (a -> a -> a) -> a -> f a
chainl pa pop a = pure a <|> chainl1 pa pop

chainl1 :: forall f a. Alternative f => f a -> f (a -> a -> a) -> f a
chainl1 pa pop = pa <|>  ((&) <$> pa <*> loop)
  where
    -- note: loop on function instead of value
    -- i.e. can't do something like
    --    
    --    loopBad :: f a -> f a
    --    loopBad v = loopBad $ (&) <$> v <*> pop <*> pa
    --  
    -- since this calls the loop before parsing a single term and hence loops infinitely.
    -- Instead, we return f (a -> a) to defer the operation to the caller instead.
    loop :: f (a -> a)
    loop = apply <$> pop <*> pa <*> (pure id <|> loop)
    apply op a foldUp r = foldUp (op r a)

manyTill :: Alternative f => f a -> f a -> f [a]
manyTill pa pend = ended <|> loop
  where
    ended = pend $> []
    loop = (:) <$> pa <*> (ended <|> loop)
