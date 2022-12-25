{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}

module Chapter12.TemplateProjection
  ( project_rawAst,
    project_withExpr,
    projectionDeclare,
    projectionDeclareMany,
    toTupleE,
    toTupleDeclare,
    toTupleDeclareMany,
    sumInner,
  )
where

import Control.Monad
import Data.Foldable
import Data.List (singleton)
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

projectionDeclareMany :: Int -> Int -> Q [Dec]
projectionDeclareMany start end = declareRange declareNTuple [start .. end]
  where
    declareNTuple n = mapSum (projectionDeclare n) [0 .. n - 1]

declareRange :: (Int -> Q [Dec]) -> [Int] -> Q [Dec]
declareRange = mapSum

projectionDeclare :: Int -> Int -> Q [Dec]
projectionDeclare n k = (:) <$> typeDec <*> [d|$(varP projName) = $(project_rawAst n k)|]
  where
    typeDec = sigD projName (projectionTypeSignature n k)
    projName = makeName n k

mapSum :: forall a t m s. (Traversable t, Monad m, Monoid s) => (a -> m s) -> t a -> m s
mapSum f r = fold <$> traverse f r

sumInner :: (Monad m, Monoid a) => m a -> m a -> m a
sumInner x y = (<>) <$> x <*> y

projectionTypeSignature :: Int -> Int -> Q Type
projectionTypeSignature n k = do
  names <- replicateM n $ newName "x"
  nameVars <- traverse (`plainInvisTV` specifiedSpec) names
  let tuples = toTupleType names
      selected = names !! k
  forallT nameVars (return []) [t|$(return tuples) -> $(varT selected)|]

toTupleName :: Int -> Name
toTupleName n = mkName $ "toTuple_" +|| n ||+ ""

toTupleDeclareMany :: Int -> Int -> Q [Dec]
toTupleDeclareMany start end = declareRange toTupleDeclare [start .. end]

toTupleDeclare :: Int -> Q [Dec]
toTupleDeclare n = do
  names <- replicateM n $ newName "x"
  let (pat, expr) = toTupleInfo names
      mainClause = Clause [pat] (NormalB expr) []
      errorClause = clause [return WildP] (normalB errorExp) []
  singleton <$> funD name [return mainClause, errorClause]
  where
    name = toTupleName n
    errorExp = [|error $(litE (StringL $ "List must be exactly length " +| n |+ ""))|]

toTupleType :: [Name] -> Type
toTupleType names = foldl AppT (TupleT (length names)) $ VarT <$> names

toTupleInfo :: [Name] -> (Pat, Exp)
toTupleInfo names =
  ( ListP $ VarP <$> names,
    TupE $ Just . VarE <$> names
  )

toTupleE :: Int -> Q Exp
toTupleE n = do
  names <- replicateM n $ newName "x"
  let (lpat, expr) = toTupleInfo names
  return $ LamE [lpat] expr
