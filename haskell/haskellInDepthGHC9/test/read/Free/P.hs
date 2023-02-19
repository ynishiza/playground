{-# LANGUAGE LambdaCase #-}

module Free.P
  ( P,
    run,
    int,
    char,
    peek,
    parserToP,
    pToParser,
  )
where

import Control.Monad
import Control.Monad.Free.Church
import Base.Parse (Parse (..)) 
import Base.Parse qualified as PB
import Base.Combinators qualified as PB

type P a = F Parse a

pToParser :: P a -> Parse a
pToParser (F f) = f pure join

parserToP :: Parse a -> P a
parserToP = liftF

char :: P Char
char = liftF $ Get pure

peek :: P String
peek = liftF $ Look pure

digit :: P Int
digit = parserToP PB.digit

stop :: P a
stop = liftF Stop

int :: P Int
int =
  peek >>= \case
    (_ : xs) ->
      foldr
        ( \_ aggr -> do
            r <- aggr
            x <- digit
            return $ x + 10 * r
        )
        digit
        xs
    _ -> stop

run :: P a -> String -> [(a, String)]
run = PB.parse . pToParser
