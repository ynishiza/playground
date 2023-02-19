{-# LANGUAGE RankNTypes #-}

module P
  ( P,
    ParseBuilder (..),
    parse,
    pToParser,
    parserToP,
    get,
    look,
    stop,
    result,
  )
where

import Base.Parse (Parse (..))
import Base.Parse qualified as PB
import Control.Applicative ((<|>), Alternative(..))
import Control.Monad

type P = ParseBuilder

newtype ParseBuilder a = ParseBuilder {runParseBuilder :: forall b. (a -> Parse b) -> Parse b}

instance Functor ParseBuilder where
  fmap f (ParseBuilder p) = ParseBuilder $ \k -> p (k . f)

instance Applicative ParseBuilder where
  pure a = ParseBuilder $ \k -> k a
  (<*>) = ap

instance Monad ParseBuilder where
  (ParseBuilder p) >>= h = ParseBuilder $ \k -> p (\a -> runParseBuilder (h a) k)

instance Alternative ParseBuilder where
  empty = stop
  (ParseBuilder p) <|> (ParseBuilder q) = ParseBuilder $ \k -> p k <|> q k

instance MonadFail ParseBuilder where
  fail _ = stop

instance MonadPlus ParseBuilder where

parserToP :: Parse a -> P a
parserToP p = ParseBuilder $ \k -> p >>= k

pToParser :: P a -> Parse a
pToParser (ParseBuilder p) = p pure

parse :: P a -> String -> [(a, String)]
parse p = PB.parse (pToParser p)

get :: P Char
get = ParseBuilder Get

look :: P String
look = ParseBuilder Look

stop :: P a
stop = ParseBuilder $ const Stop

result :: a -> P a -> P a
result a (ParseBuilder rest) = ParseBuilder $ \k -> k a <|> rest k
