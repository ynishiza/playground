#!/usr/bin/env stack
{-# LANGUAGE DeriveAnyClass #-}
{-
  Run with
    stack exec -- src/scratch/<name>.hs
    stack ghci -- src/scratch/<name>.hs
-}
{-# LANGUAGE DerivingStrategies #-}

import Control.Applicative
import Control.Exception (Exception)
import GHC.Generics (Generic)
import Text.Megaparsec
import Text.Megaparsec.Char

newtype MyException = MyException String
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (Exception)

p :: Parsec MyException String String
p = string "abc" <|> string "def"

main :: IO ()
main = do
  print $ parse p "test" "abc"
  print $ parse p "test" "def"
  print $ parse p "test" "dzzzzz"
  return ()
