{-# LANGUAGE OverloadedStrings #-}

module Chapter12.TemplateProjection
  ( project,
  )
where

import Fmt
import Language.Haskell.TH

project :: Int -> Int -> Q Exp
project n k
  | n <= 0 = errMsg
  | k < 0 = errMsg
  | k >= n = errMsg
  | otherwise = do
    x <- newName "x"
    let g i = if i == k then VarP x else WildP
    pure $ LamE [TupP $ g <$> [0 .. (n -1)]] (VarE x)
  where
    errMsg = fail $ "n=" +| n |+ " k=" +| k |+ " does not satisfy 0 <= k < n"
