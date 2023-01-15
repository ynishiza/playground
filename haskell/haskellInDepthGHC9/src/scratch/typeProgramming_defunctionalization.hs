{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

import Data.Kind
import Data.Singletons.TH
import GHC.TypeLits (Nat)
import Prelude.Singletons

type F :: Nat -> Nat -> Nat
type family F a b where
  F x y = x + y

type FSym0 :: TyFun Nat (TyFun Nat Nat -> Type) -> Type
-- type FSym0 :: (Nat ~> (Nat ~> Nat))
data FSym0 a

type FSym1 :: Nat -> TyFun Nat Nat -> Type
-- type FSym1 :: Nat -> (Nat ~> Nat)
data FSym1 a b'

type instance Apply FSym0 x = FSym1 x

type instance Apply (FSym1 x) y = F x y

type A :: Nat
type A = Foldr FSym0 0 '[1, 2, 3]

type B :: Nat
-- type B = FoldrSym0 @@ FSym0 @@ 0 @@ '[1, 2]
type B = FoldrSym0 `Apply` FSym0 `Apply` 0 `Apply` '[1, 2]

type C :: [Nat]
type C = Map (FSym1 10) '[1, 2, 3]
