{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Chapter11.DataKindsCanonical () where

import Data.Kind
import GHC.TypeLits

data MyNat = Zero | Succ !MyNat
data List a = Nil | Cons !a !(List a)
data Pair a b = MkPair !a !b
data Sum a b = L !a | R !b

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
