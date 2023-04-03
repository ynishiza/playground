-- From "Template Meta-Programming for Haskell"
-- https://www.microsoft.com/en-us/research/publication/template-meta-programming-for-haskell
--
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use section" #-}

module Template.Printf (printf) where

import Data.Attoparsec.ByteString.Char8 hiding (D)
import Data.ByteString.Char8 qualified as B
import Language.Haskell.TH
import Template.PrintfParser

printf :: String -> Q Exp
printf str =
  either
    (\s -> fail $ "Failed to parse string " <> s)
    (flip compute [|""|])
    v
  where
    v = parseOnly parseFormats (B.pack str)

compute :: [Format] -> Q Exp -> Q Exp
compute [] str = str
compute ((Literal l) : xs) str = compute xs [|$str <> l|]
compute (D : xs) str = [|\(v :: Int) -> $(compute xs [|$str <> show v|])|]
compute (S : xs) str = [|\v -> $(compute xs [|$str <> v|])|]

