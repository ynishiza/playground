{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}

module Chapter11.TypeFamilySynonymsEg1_1
  ( Simplify,
    CanSimplify (..),
  )
where

import Data.Kind
import Data.Word

type Simplify :: Type -> Type
type family Simplify a

type instance Simplify Int = Word8

type instance Simplify Double = Word8

type family Simplify2 (a :: Type) :: Type where
  Simplify2 Int = Word8
  Simplify2 Double = Word8

type CanSimplify :: Type -> Constraint
class CanSimplify a where
  simplify :: a -> Simplify a

instance CanSimplify Int where simplify = fromIntegral

instance CanSimplify Double where simplify = round
