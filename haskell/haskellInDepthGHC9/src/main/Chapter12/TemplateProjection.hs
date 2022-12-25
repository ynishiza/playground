{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}

module Chapter12.TemplateProjection
  ( project_rawAst,
    project_withExpr,
    declareProjection,
    makeTypeSignature,
    sumInner,
  )
where

import Data.List (singleton)
import Control.Monad
import Fmt
import Language.Haskell.TH

project_rawAst :: Int -> Int -> Q Exp
project_rawAst = projectBaseWithError f
  where
    f n k = do
      varX <- newName "x"
      let var i = if i == k then VarP varX else WildP
      pure $ LamE [TupP $ var <$> [0 .. (n - 1)]] (VarE varX)

project_withExpr :: Int -> Int -> Q Exp
project_withExpr = projectBaseWithError f
  where
    f n k = do
      varX <- newName "x"
      let var i = if i == k then varP varX else wildP
          pattern :: Q Pat
          pattern = tupP $ var <$> [0 .. n - 1]
          value :: Q Exp
          value = varE varX
      [|\ $pattern -> $value|]

projectBaseWithError :: (Int -> Int -> Q Exp) -> Int -> Int -> Q Exp
projectBaseWithError f n k = projectBase f errMsg n k
  where
    errMsg = fail $ "n=" +| n |+ " k=" +| k |+ " does not satisfy 0 <= k < n"

projectBase :: (Int -> Int -> Q Exp) -> Q Exp -> Int -> Int -> Q Exp
projectBase f errMsg n k
  | n <= 0 = errMsg
  | k < 0 = errMsg
  | k >= n = errMsg
  | otherwise = f n k

makeName :: Int -> Int -> Name
makeName n k = mkName $ "proj_" +| n |+ "_" +| k |+ ""

declareProjection :: Int -> Int -> Q [Dec]
-- declareProjection n k = [d| $(varP $ makeName n k) = $(project_rawAst n k) |]
declareProjection n k = (:) <$> typeDec <*> [d| $(varP nm) = $(project_rawAst n k) |]
  where
    typeDec = sigD nm (makeTypeSignature n k)
    nm = makeName n k

sumInner :: (Monad m, Monoid a) => m a -> m a -> m a
sumInner x y = (<>) <$> x <*> y

makeTypeSignature :: Int -> Int -> Q Type
makeTypeSignature n k = do
  names <- replicateM n $ newName "x"
  nameVars <- traverse (`plainInvisTV` specifiedSpec) names
  let 
    tuples = foldl AppT (TupleT n) $ VarT <$> names
    sign = AppT (AppT ArrowT tuples) (VarT $ names!!k)
  forallT nameVars (return []) (return sign)

-- declareProjection :: Int -> Int -> Q Dec
-- declareProjection n k = do
--   f <- project_rawAst n k
--   return $ ValD (VarP $ makeName n k) (NormalB f) []
  
  -- [d| $(return [ValD (VarP x) (NormalB (VarE x)) []]) |]
  -- [d| $(VarP $ makeName n k) = $(project_rawAst n k) |]
