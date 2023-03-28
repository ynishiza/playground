-- Run
--    stack ghci -- src/typelevel/defunctionalization.hs
--
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

import Data.Kind
import Data.Singletons.TH
import GHC.TypeLits (Nat)
import Prelude.Singletons

type Fm :: Nat -> Nat -> Nat
type family Fm a b where
  Fm x y = x + y

type FSym0 :: TyFun Nat (TyFun Nat Nat -> Type) -> Type
-- type FSym0 :: (Nat ~> (Nat ~> Nat))
data FSym0 a

type FSym1 :: Nat -> TyFun Nat Nat -> Type
-- type FSym1 :: Nat -> (Nat ~> Nat)
data FSym1 a b'

type instance Apply FSym0 x = FSym1 x

type instance Apply (FSym1 x) y = Fm x y

type A :: Nat
type A = Foldr FSym0 0 '[1, 2, 3]

type B :: Nat
type B = FoldrSym0 @@ FSym0 @@ 0 @@ '[1, 2]
-- type B = FoldrSym0 `Apply` FSym0 `Apply` 0 `Apply` '[1, 2]

type C :: [Nat]
type C = Map (FSym1 10) '[1, 2, 3]

type E :: (Bool, Bool)
type E = '( 'True, 'False)

type F :: [Nat]
type F = Filter ((<@#@$$) 1) '[1, 2, 3, 4]

type F1 :: [Nat]
type F1 = Map ((+@#@$$) 1) '[1, 2, 3, 4]

type Identity :: Bool
type Identity = 1 == 1

type B2 :: Bool
type B2 = 1 < 2 && 1 > 0

type AbsList :: [Nat] -> [Nat]
type family AbsList l where
  AbsList l = Map AbsSym0 l

type R1 :: Bool
type R1 = F == C 

type R2 = Maybe_ 0 IdSym0 ('Just 1)
type R22= Maybe_ 0 IdSym0 'Nothing :: Nat
type R23= Either_ ((+@#@$$) 1) ((+@#@$$) 2) ('Left 1)

type T1 = "A" <> "B" :: Symbol
type T2 = 1 <> 2 :: Nat
type T3 = '[1] <> '[] :: [Nat]

type T4 = 1 + Negate (Negate 2)
type T5 = Fmap ((*@#@$$) 10) '[1, Negate 2]
