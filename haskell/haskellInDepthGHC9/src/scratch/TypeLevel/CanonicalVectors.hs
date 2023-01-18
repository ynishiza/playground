{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}

module TypeLevel.CanonicalVectors
  ( Vec (..),
    vHead,
    vElem,
    vLength,
    prevNat,
    module X,
  )
where

import Data.Kind
import Data.Type.Nat as X
import Data.Type.Nat.LE as X
import Data.Void

type Vec :: forall a. a -> Nat -> Type
data Vec a n where
  VNil :: Vec a 'Z
  VCons :: SNatI n => a -> Vec a n -> Vec a ('S n)

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
vElem VNil _ = absd (leProof @('S m))

absd :: forall m a. LEProof ('S m) 'Z -> a
absd pf = absurd $ leSwap' (LEZero @m) pf
