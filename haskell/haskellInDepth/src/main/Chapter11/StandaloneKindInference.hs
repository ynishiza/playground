{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeFamilies, PolyKinds #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}

module Chapter11.StandaloneKindInference
  (
  )
where

import Data.Functor.Identity
import Data.Kind
import Data.Proxy
import GHC.TypeLits

newtype Apply1 m = Apply1 (m Int)

newtype Apply2 m = Apply2 (m Int Int)

-- arity T1 = 1
type T1 :: forall a. a -> Type
type T1 = Proxy
t1 :: T1 @Type Int
t1 = Proxy
t1b :: T1 @(Type -> Type) Maybe
t1b = Proxy
t1c :: T1 @(Type -> Type -> Type) Either
t1c = Proxy

-- arity T2 = 2
type T2 :: forall a. a -> Type
type T2 a = Proxy a
t2 :: T2 @Type Int
t2 = Proxy
t2b :: T2 @(Type -> Type) Maybe
t2b = Proxy
t2c :: T2 @(Type -> Type -> Type) Either
t2c = Proxy

-- arity F0 = 0
type F0 :: forall a. a -> Type
type family F0 :: forall b. b -> Type where
  F0 = Proxy
-- type instance F0 = Maybe       ERROR
t3 :: F0 Int
t3 = Proxy
t3b :: F0 Maybe
t3b = Proxy

-- arity F0b = 0
type family F0b :: b  -> Type where
  F0b = Proxy
  -- F0b = Maybe                  ERROR

-- arity F1 = 1
type F1 :: forall a. a -> Type
type family F1 :: b -> Type where
-- type family F1 :: Type -> Type        -- ERROR
  F1 @Type = Maybe
  F1 @(Type -> Type) = Apply1
  F1 @(Type -> Type -> Type) = Apply2
t4 :: F1 @Type Int
t4 = Just 1
t4b :: F1 @(Type -> Type) Maybe
t4b = Apply1 (Just 1)
t4c :: F1 @(Type -> Type -> Type) Either
t4c = Apply2 (Right 1)

-- arity F2 = 2
type F2 :: forall a. a -> Type
type family F2 a :: Type where
  F2 @Type Int = Int
  F2 @(Type -> Type) Maybe = Bool
t5 :: F2 @Type Int
t5 = 1
t5b :: F2 @(Type -> Type) Maybe
t5b = False


type Append :: forall a. [a] -> [a] -> [a]
type family Append x y :: [a] where
  Append @Nat [1,2] [3,4] = [1,2,3,4]
a1 :: Proxy (Append [1,2] [3,4])
a1 = Proxy

type MaybeIf :: Bool -> (Type -> Type)
type family MaybeIf b where
  MaybeIf 'True = Maybe
  MaybeIf 'False = Identity

-- type B :: (Type -> Type -> Type) -> Type -> Type -> Type
type B :: forall a b. (a -> b -> Type) -> a -> b -> Type
data B f a b = B (f a b)
type T = B Either Int ()
x :: T
x = B (Right ())

type A :: forall a b. (b -> Type) -> (a -> b) -> a -> Type
data A f g a = A (f (g a))
type A1 = A [] [] Int
xa :: A1 
xa = A [[1]]

type S :: forall a. (a -> Type) -> (a -> Type) -> a -> Type
data S l r a = L !(l a) | R !(r a)
