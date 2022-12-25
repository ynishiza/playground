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
    varX <- newName "x"
    let var i = if i == k then VarP varX else WildP
    pure $ LamE [TupP $ var <$> [0 .. (n -1)]] (VarE varX)
  where
    errMsg = fail $ "n=" +| n |+ " k=" +| k |+ " does not satisfy 0 <= k < n"
