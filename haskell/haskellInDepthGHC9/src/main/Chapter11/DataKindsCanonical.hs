{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Chapter11.DataKindsCanonical (
  MyNat(..)
) where

import Data.Kind
import GHC.TypeLits
import GHC.Generics

type MyNat :: Type
data MyNat = Zero | Succ !MyNat deriving (Show, Generic, Eq)

type List :: Type -> Type
data List a = Nil | Cons !a !(List a)

type Pair :: Type -> Type -> Type
data Pair a b = MkPair !a !b

type Sum :: Type -> Type -> Type
data Sum a b = L !a | R !b

type Vec :: Type -> MyNat -> Type
data Vec a n where
  VNil :: a -> Vec a 'Zero
  VCons :: a -> Vec a n -> Vec a ('Succ n)

type N0 :: MyNat
type N0 = 'Zero
type N1 :: MyNat
type N1 = 'Succ ('Succ 'Zero)

type L0 :: forall a. List a
type L0 = 'Nil
type L1 :: List Bool
type L1 = 'Cons 'True ('Cons 'False 'Nil)
type L2 :: List Symbol
type L2 = 'Cons "ABC" ('Cons "DEF" 'Nil)
type L3 :: List Nat
type L3 = 'Cons 1 ('Cons 1 'Nil)
type L4 :: List Type
type L4 = 'Cons Int ('Cons (List Int) 'Nil)

-- mynat :: Proxy 'Zero -> Int
-- mynat _ = 0
-- mynat :: forall (a :: MyNat). Proxy ('Succ a) -> Int
-- mynat _ = 1 + mynat (Proxy :: Proxy a)
-- mynat :: Proxy (a :: MyNat) -> Integer
-- mynat _ = 0
-- mynat (Succ
-- mynat :: M1 i c (f :+: g) p -> Integer
