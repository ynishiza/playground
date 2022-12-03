{-# LANGUAGE FlexibleInstances #-}
-- {-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}

module Chapter11.TypeFamilySynonymsEg1
  ( Simplify,
    CanSimplify (..),
  )
where

import Data.Kind
import Data.Word

-- type Simplify :: Type -> Type                 -- StandaloneKindSignatures
type family Simplify (a :: Type) :: Type

-- type family Simplify (a :: k) :: k

type instance Simplify Int = Word8

type instance Simplify Double = Word8

type instance Simplify Bool = Char

type instance Simplify String = String

type instance Simplify Char = String

type instance Simplify (Maybe a) = String

type family Simplify2 (a :: Type) :: Type where
  Simplify2 Int = Word8
  Simplify2 Double = Word8
  Simplify2 Bool = Char
  Simplify2 String = String
  Simplify2 Char = String
  forall a. Simplify2 (Maybe a) = String
-- Simplify2 (Maybe a) = String
  forall a. Simplify2 a = String

-- type CanSimplify :: Type -> Constraint        StandaloneKindSignatures
class CanSimplify (a :: Type) where
  simplify :: a -> Simplify a

instance CanSimplify Int where simplify = fromIntegral

instance CanSimplify Double where simplify = round

instance CanSimplify Bool where simplify True = 'T'; simplify False = 'F'

instance CanSimplify Char where simplify = (: [])

instance CanSimplify String where simplify = id

instance CanSimplify (Maybe a) where simplify _ = "Maybe"
