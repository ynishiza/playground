{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PolyKinds #-}
-- {-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}

module Chapter11.TypeFamilySynonymsEg2
  ( 
    Widen,
    CanWiden (..),
  )
where

import Data.Kind

-- type Widen :: forall a. a -> a        StandaloneKindSignatures
-- type Widen :: Type -> Type            StandaloneKindSignatures
type family Widen (a :: k) :: k where
  Widen Double = Double
  Widen Int = Double
  Widen Char = String
  forall a. Widen a = String

-- type CanWiden :: Type -> Constraint    StandaloneKindSignatures
class CanWiden (a :: Type) where
  widen :: a -> Widen a

instance CanWiden Double where widen = id

instance CanWiden Int where widen = fromIntegral

instance CanWiden Bool where widen = show

instance CanWiden Char where widen = (: [])

instance CanWiden String where widen = id
