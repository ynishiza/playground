{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}
{-# HLINT ignore "Use section" #-}

module Chapter12.Template.Projection
  ( project_rawAst,
    project_withExpr,
    projectionDeclare,
    projectionDeclareMany,
    myRandomValueDeclare,
    myRandomValueDeclare2,
    MyTemplateError (..),
  )
where

import Chapter12.Template.Utils
import Control.Monad
import Fmt
import Language.Haskell.TH

projectionDeclareMany :: Int -> Int -> Q [Dec]
projectionDeclareMany start end = foldMap declareNTuple [start .. end]
  where
    declareNTuple n = foldMap (projectionDeclare n) [0 .. n - 1]

projectionDeclare :: Int -> Int -> Q [Dec]
projectionDeclare n k = do
  x <- newNameX
  let (_, _, c) = projectInfo n k x
  sig <- sigD projName (projectionTypeSignature n k)
  impl <- funD projName [return c]
  return [sig, impl]
  where
    projName = makeProjName n k

project_rawAst :: Int -> Int -> Q Exp
project_rawAst = projBaseWithError f
  where
    f n k = do
      varX <- newNameX
      let (p, e, _) = projectInfo n k varX
      pure $ LamE [p] e

project_withExpr :: Int -> Int -> Q Exp
project_withExpr = projBaseWithError f
  where
    f n k = do
      varX <- newNameX
      let var i = if i == k then varP varX else wildP
          pattern :: Q Pat
          pattern = tupP $ var <$> [0 .. n - 1]
          value :: Q Exp
          value = varE varX
      [|\ $pattern -> $value|]

projectInfo :: Int -> Int -> Name -> (Pat, Exp, Clause)
projectInfo n k name = (pat, expr, Clause [pat] (NormalB expr) [])
  where
    pat = TupP $ var <$> [0 .. n - 1]
    expr = VarE name
    var i = if i == k then VarP name else WildP

projBaseWithError :: (Int -> Int -> Q Exp) -> Int -> Int -> Q Exp
projBaseWithError f n k = projBase f errMsg n k
  where
    errMsg = fail $ "n=" +| n |+ " k=" +| k |+ " does not satisfy 0 <= k < n"

projBase :: (Int -> Int -> Q Exp) -> Q Exp -> Int -> Int -> Q Exp
projBase f errMsg n k
  | n <= 0 = errMsg
  | k < 0 = errMsg
  | k >= n = errMsg
  | otherwise = f n k

makeProjName :: Int -> Int -> Name
makeProjName n k = mkName $ "proj_" +| n |+ "_" +| k |+ ""

projectionTypeSignature :: Int -> Int -> Q Type
projectionTypeSignature n k = do
  names <- replicateM n $ newName "a"
  simpleSignature
    names
    emptyCxt
    [t|$(pure $ tupleSignature names) -> $(varT $ names !! k)|]

myRandomValueDeclare :: Q [Dec]
myRandomValueDeclare = do
  s <- sigD name $ conT (mkName "Int")
  e <- funD name [clause [] (normalB $ litE $ IntegerL 11) []]
  pure [s, e]
  where
    name = mkName "someVeryRandomValue"

myRandomValueDeclare2 :: Q [Dec]
myRandomValueDeclare2 = [d|someVeryRandomValue2 :: Int; someVeryRandomValue2 = 22|]
