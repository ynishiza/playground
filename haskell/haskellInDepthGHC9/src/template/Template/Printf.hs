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
compute [] resultText = resultText
compute ((Literal l) : formats) resultText = compute formats [|$resultText <> l|]
compute (D : formats) resultText = [|\(v :: Int) -> $(compute formats [|$resultText <> show v|])|]
compute (S : formats) resultText = [|\v -> $(compute formats [|$resultText <> v|])|]

