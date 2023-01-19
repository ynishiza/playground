{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}

module TypeLevel.CanonicalVectors
  ( Vec (..),
    SomeVec (..),
    vSomeCons,
    vFromList,
    vToList,
    vHead,
    vElem,
    vLength,
    prevNat,
    vNull,
    vMap,
    vFoldr,
    vEq,
    vRepeat,
    vAppend,
    vCycle,
    withVecSNat,
    withVecSNatI,
    module X,
  )
where

import Data.Kind
import Data.Type.Dec as X
import Data.Type.Equality
import Data.Type.Nat as X
import Data.Type.Nat.LE as X
import Data.Void

type Vec :: forall k. k -> Nat -> Type
data Vec a n where
  VNil :: Vec a 'Z
  VCons :: SNatI n => a -> Vec a n -> Vec a ('S n)

type SomeVec :: forall k. k -> Type
data SomeVec a where
  MkSomeVec :: forall a n. Vec a n -> SomeVec a

instance Show a => Show (Vec a n) where
  show VNil = "[]"
  show v@(VCons _ _) = show $ vToList v

instance Show a => Show (SomeVec a ) where
  show (MkSomeVec v) = show v

instance Eq a => Eq (Vec a n) where
  VNil == VNil = True
  (VCons v vs) == (VCons w ws)  = v == w && vs == ws

withVecSNatI :: Vec a n -> (SNatI n => r) -> r
withVecSNatI v = withSNat (withVecSNat v)

withVecSNat :: Vec a n -> SNat n
withVecSNat VNil = SZ
withVecSNat (VCons _ _) = snat

vToList :: Vec a n -> [a]
vToList VNil = []
vToList (VCons x xs) = x : vToList xs

vFromList :: [a] -> SomeVec a
vFromList = foldr vSomeCons (MkSomeVec VNil)

vSomeCons :: forall a. a -> SomeVec a -> SomeVec a
vSomeCons x (MkSomeVec v) = withSNat (vSNat v) $ MkSomeVec (VCons x v)

vSNat :: Vec a n -> SNat n
vSNat VNil = SZ
vSNat (VCons _ _) = snat

vLength :: forall a n. Vec a n -> Integer
vLength VNil = 0
vLength (VCons _ _) = fromIntegral $ snatToNatural $ snat @n

vHead :: Vec a ('S n) -> a
vHead (VCons v _) = v

prevNat :: SNat ('S n) -> SNat n
prevNat n@SS = withSNat n snat

vElem :: forall a n m. LE ('S m) n => Vec a n -> SNat m -> a
vElem (VCons v _) SZ = v
vElem (VCons _ x) m@SS = withLEProof leCondition $ vElem x m'
  where
    leCondition :: 'S n' ~ n => LEProof m n'
    leCondition = lePred leProof
    m' :: 'S m' ~ m => SNat m'
    m' = withSNat m snat
-- case: Vector can't be empty
vElem VNil _ = absurd $ leSwap' LEZero (leProof @('S m))

vNull :: forall a n. Vec a n -> Dec (n :~: 'Z)
vNull VNil = Yes Refl
vNull (VCons _ _) = No $ \case {}

vEq :: Eq a => Vec a n -> Vec a m -> Bool
vEq VNil VNil = True
vEq VNil _ = False
vEq _ VNil = False
vEq (VCons x xs) (VCons y ys) = x == y && vEq xs ys

vMap :: (a -> b) -> Vec a n -> Vec b n
vMap _ VNil = VNil
vMap f (VCons x xs) = VCons (f x) $ vMap f xs

vFoldr :: (a -> b -> b) -> b -> Vec a n -> b
vFoldr _ x0 VNil = x0
vFoldr f x0 (VCons x xs) = f x $ vFoldr f x0 xs

vRepeat :: a -> SNat n -> Vec a n 
vRepeat _ SZ = VNil
vRepeat x SS = VCons x $ vRepeat x snat

type NAppend :: Nat -> Nat -> Nat
type family NAppend a b where
  NAppend 'Z a = a
  NAppend ('S b) a = 'S (NAppend b a)

withNAppend :: SNat n -> SNat m -> (SNatI (NAppend n m) => r) -> r
withNAppend SZ m f = withSNat m f
withNAppend n@SS m f = withNAppend (prevNat n) m f

vAppend :: forall a n m. Vec a n -> Vec a m -> Vec a (NAppend n m)
vAppend VNil v = v
vAppend (VCons x xs) v = withNAppend (prevNat (snat @n)) (withVecSNat v) $ VCons x (vAppend xs v)

type VMult :: Nat -> Nat -> Nat
type family VMult n a where
  VMult 'Z a = 'Z
  VMult ('S n) a = NAppend a (VMult n a)

vCycle :: Vec a n -> SNat m -> Vec a (VMult m n)
vCycle _ SZ = VNil
vCycle v m@SS = vAppend v (vCycle v (prevNat m))
