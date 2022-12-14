{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}

module Chapter11.StandaloneKindInference
  ( F0D (..),
  )
where

import Data.Kind

data F0D b = F0D

type F0 :: forall a. a -> Type
type family F0 :: forall a. a -> Type

type instance F0 = F0D

-- type instance F0 = Maybe

type F1 :: forall a. a -> Type
type family F1 :: a -> Type

type instance F1 = Maybe

type F2 :: forall a. a -> Type
type family F2 a :: Type

type instance F2 Bool = Int

type instance F2 Maybe = Int

