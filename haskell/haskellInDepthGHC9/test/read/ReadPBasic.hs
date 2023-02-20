{-# LANGUAGE LambdaCase #-}

module ReadPBasic
  ( digit,
    int,
    div,
    bool,
    and,
    or,
    andOr,
  )
where

import Control.Applicative ((<|>))
import Text.ParserCombinators.ReadP
import Prelude hiding (and, div, or)

digit :: Integral n => ReadP n
digit =
  get >>= \case
    '1' -> pure 1
    '2' -> pure 2
    '3' -> pure 3
    '4' -> pure 4
    '5' -> pure 5
    '6' -> pure 6
    '7' -> pure 7
    '8' -> pure 8
    '9' -> pure 9
    '0' -> pure 0
    _ -> pfail

int :: Integral n => ReadP n
int = do
  (v : vs) <- many1 digit
  return $ foldl (\xs x -> 10 * xs + x) v vs

div :: Fractional n => ReadP (n -> n -> n)
div = char '/' >> return (/)

rpTrue :: ReadP Bool
rpTrue = string "True" >> return True

rpFalse :: ReadP Bool
rpFalse = string "False" >> return False

bool :: ReadP Bool
bool = rpTrue <|> rpFalse

and :: ReadP (Bool -> Bool -> Bool)
and = string "&&" >> return (&&)

or :: ReadP (Bool -> Bool -> Bool)
or = string "||" >> return (||)

andOr :: ReadP (Bool -> Bool -> Bool)
andOr = and <|> or
