-- {-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}

module Chapter11.TypeFamilySynonymsEg3
  ( Simplify,
  )
where

import Data.Kind
import Data.Word

type family Simplify (a :: Type) :: Type where
  Simplify Int = Word8
  Simplify Double = Word8
  Simplify Bool = Char
  Simplify String = String
  Simplify Char = String
  forall a. Simplify (Maybe a) = String
-- Simplify (Maybe a) = String
  forall a. Simplify a = String
