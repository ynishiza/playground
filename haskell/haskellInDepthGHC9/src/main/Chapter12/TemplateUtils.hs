{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Chapter12.TemplateUtils
  ( emptyCxt,
    newNameX,
    TypeVar,
    declareRange,
    mapSum,
    sumInner,
    MyTemplateError (..),
    myTemplateErrorE,
    tupleSignature,
    listSignature,
    simpleSignature,
  )
where

import Control.Exception
import Data.Foldable
import Language.Haskell.TH

emptyCxt :: Q Cxt
emptyCxt = pure []

newNameX :: Q Name
newNameX = newName "x"

type TypeVar = Type

newtype MyTemplateError = MkMyTemplateError String
  deriving stock (Show, Eq)
  deriving anyclass (Exception)

myTemplateErrorE :: String -> Q Exp
myTemplateErrorE msg = [|throw (MkMyTemplateError $(litE (StringL msg)))|]

declareRange :: (Int -> Q [Dec]) -> [Int] -> Q [Dec]
declareRange = mapSum

mapSum :: forall a t m s. (Traversable t, Monad m, Monoid s) => (a -> m s) -> t a -> m s
mapSum f r = fold <$> traverse f r

sumInner :: (Monad m, Monoid a) => m a -> m a -> m a
sumInner x y = (<>) <$> x <*> y

tupleSignature :: [Name] -> Type
tupleSignature names = foldl AppT (TupleT (length names)) $ VarT <$> names

listSignature :: Name -> Type
listSignature = AppT ListT . VarT

simpleSignature :: [Name] -> Q Cxt -> Q Type -> Q Type
simpleSignature names = forallT nv
  where
    nv = flip PlainTV specifiedSpec <$> names
