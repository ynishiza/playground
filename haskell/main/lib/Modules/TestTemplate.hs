{-# LANGUAGE OverloadedStrings #-}

module Modules.TestTemplate
  ( myProject,
  )
where

import Fmt
import Language.Haskell.TH

myProject :: Int -> Int -> Q Exp
myProject n k
  | n <= 0 = errMsg
  | k < 0 = errMsg
  | k >= n = errMsg
  | otherwise = do
    x <- newName "x"
    let g i = if i == k then VarP x else WildP
    pure $ LamE [TupP $ g <$> [0 .. (n -1)]] (VarE x)
  where
    errMsg = fail $ "n=" +| n |+ " k=" +| k |+ " does not satisfy 0 <= k < n"
