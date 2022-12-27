{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}
{-# HLINT ignore "Use section" #-}

module Chapter12.TemplateProjection
  ( project_rawAst,
    project_withExpr,
    projectionDeclare,
    projectionDeclareMany,
    toTupleE,
    toTupleDeclare,
    toTupleDeclareMany,
    sumInner,
    myRandomValueDeclare,
    myRandomValueDeclare2,
    MyTemplateError (..),
  )
where

import Control.Exception
import Control.Monad
import Data.Foldable
import Fmt
import Language.Haskell.TH
import Language.Haskell.TH.Syntax

newtype MyTemplateError = MkMyTemplateError String
  deriving stock (Show, Eq)
  deriving anyclass (Exception)

projectionDeclareMany :: Int -> Int -> Q [Dec]
projectionDeclareMany start end = declareRange declareNTuple [start .. end]
  where
    declareNTuple n = mapSum (projectionDeclare n) [0 .. n - 1]

toTupleDeclareMany :: Int -> Int -> Q [Dec]
toTupleDeclareMany start end = declareRange toTupleDeclare [start .. end]

declareRange :: (Int -> Q [Dec]) -> [Int] -> Q [Dec]
declareRange = mapSum

projectionDeclare :: Int -> Int -> Q [Dec]
projectionDeclare n k = do
  sig <- sigD projName (projectionTypeSignature n k)
  impl <- [d|$(varP projName) = $(project_rawAst n k)|]
  return (sig : impl)
  where
    projName = makeProjName n k

mapSum :: forall a t m s. (Traversable t, Monad m, Monoid s) => (a -> m s) -> t a -> m s
mapSum f r = fold <$> traverse f r

sumInner :: (Monad m, Monoid a) => m a -> m a -> m a
sumInner x y = (<>) <$> x <*> y

project_rawAst :: Int -> Int -> Q Exp
project_rawAst = projBaseWithError f
  where
    f n k = do
      varX <- qNewName "x"
      let var i = if i == k then VarP varX else WildP
      pure $ LamE [TupP $ var <$> [0 .. (n - 1)]] (VarE varX)

project_withExpr :: Int -> Int -> Q Exp
project_withExpr = projBaseWithError f
  where
    f n k = do
      varX <- qNewName "x"
      let var i = if i == k then varP varX else wildP
          pattern :: Q Pat
          pattern = tupP $ var <$> [0 .. n - 1]
          value :: Q Exp
          value = varE varX
      [|\ $pattern -> $value|]

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

type TypeVar = Type

projectionTypeSignature :: Int -> Int -> Q Type
projectionTypeSignature n k = do
  names <- replicateM n $ qNewName "x"
  let tuples = tupleSignature (VarT <$> names)
  simpleSignature names emptyCxt [t|$(pure tuples) -> $(varT $ names !! k)|]

toTupleName :: Int -> Name
toTupleName n = mkName $ "toTuple_" +|| n ||+ ""

toTupleDeclare :: Int -> Q [Dec]
toTupleDeclare n = do
  names <- replicateM n $ qNewName "x"
  let (pat, expr) = toTupleInfo names
      mainClause = Clause [pat] (NormalB expr) []
      errorClause = clause [pure WildP] (normalB errorExp) []
  sig <- sigD name $ toTupleSignature n
  impl <- funD name [pure mainClause, errorClause]
  pure [sig, impl]
  where
    name = toTupleName n
    errorExp = [|throw (MkMyTemplateError $(litE (StringL $ "List must be exactly length " +| n |+ "")))|]

toTupleSignature :: Int -> Q Type
toTupleSignature n = do
  name <- qNewName "a"
  let nameTV = VarT name
      nameTVs = replicate n nameTV
  simpleSignature
    [name]
    emptyCxt
    [t|$(pure $ listSignature nameTV) -> $(pure $ tupleSignature nameTVs)|]

tupleSignature :: [TypeVar] -> Type
tupleSignature names = foldl AppT (TupleT (length names)) names

listSignature :: TypeVar -> Type
listSignature = AppT ListT

toTupleInfo :: [Name] -> (Pat, Exp)
toTupleInfo names =
  ( ListP $ VarP <$> names,
    TupE $ Just . VarE <$> names
  )

toTupleE :: Int -> Q Exp
toTupleE n = do
  names <- replicateM n $ qNewName "x"
  let (lpat, expr) = toTupleInfo names
  pure $ LamE [lpat] expr

myRandomValueDeclare :: Q [Dec]
myRandomValueDeclare = do
  s <- sigD name $ conT (mkName "Int")
  e <- funD name [clause [] (normalB $ litE $ IntegerL 11) []]
  pure [s, e]
  where
    name = mkName "someVeryRandomValue"

myRandomValueDeclare2 :: Q [Dec]
myRandomValueDeclare2 = [d|someVeryRandomValue2 :: Int; someVeryRandomValue2 = 22|]

simpleSignature :: [Name] -> Q Cxt -> Q Type -> Q Type
simpleSignature names = forallT nv
  where
    nv = flip PlainTV specifiedSpec <$> names

emptyCxt :: Q Cxt
emptyCxt = pure []
