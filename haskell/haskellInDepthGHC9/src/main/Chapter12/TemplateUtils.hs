{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Chapter12.TemplateUtils
  ( emptyCxt,
    newNameX,
    TypeVar,
    MyTemplateError (..),
    myTemplateErrorE,
    tupleSignature,
    listSignature,
    simpleSignature,
  )
where

import Control.Exception
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

tupleSignature :: [Name] -> Type
tupleSignature names = foldl AppT (TupleT (length names)) $ VarT <$> names

listSignature :: Name -> Type
listSignature = AppT ListT . VarT

simpleSignature :: [Name] -> Q Cxt -> Q Type -> Q Type
simpleSignature names = forallT nv
  where
    nv = flip PlainTV specifiedSpec <$> names
