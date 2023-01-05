{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeOperators #-}

module Elevator.Elevator
  ( GoodFloor,
    Floor (..),
    prevFloor,
    nextFloor,
    sameFloor,
    belowFloor,
    aboveFloor,
    compareFloor,
    mkFloor,
    prevFloor',
  )
where

import Data.GADT.Compare
import Data.Kind
import Data.Type.Dec
import Data.Type.Equality
import Data.Type.Nat
import Data.Type.Nat.LE
import Fmt

type GoodFloor :: Nat -> Nat -> Constraint
type GoodFloor mx f = (SNatI f, SNatI mx, LE f mx)

type Floor :: Nat -> Nat -> Type
data Floor mx f where
  MkFloor :: GoodFloor mx f => Floor mx f

instance Show (Floor mx f) where
  show MkFloor = "Floor " +|| snat @mx ||+ " " +|| snat @f ||+ ""

instance Eq (Floor mx f) where
  x == y = show x == show y

mkFloor :: forall mx f. (SNatI mx, SNatI f) => Maybe (Floor mx f)
mkFloor = case decideLE @f @mx of
  Yes pf -> withLEProof pf $ Just MkFloor
  No _ -> Nothing

nextFloor :: forall f mx. LE ('S f) mx => Floor mx f -> Floor mx ('S f)
nextFloor MkFloor = MkFloor @mx @('S f)

prevFloor :: forall f mx. Floor mx ('S f) -> Floor mx f
prevFloor MkFloor = withLEProof boundPrf $ withSNat floorPrf MkFloor
  where
    floorPrf :: SNat f
    floorPrf = case snat @('S f) of SS -> snat
    boundPrf :: LEProof f mx
    boundPrf = leStepL (leProof @('S f) @mx)

sameFloor :: Floor mx k -> Floor mx l -> Maybe (k :~: l)
sameFloor x y = case compareFloor x y of GEQ -> Just Refl; _ -> Nothing

belowFloor :: Floor mx k -> Floor mx l -> Bool
belowFloor x y = case compareFloor x y of GLT -> True; _ -> False

aboveFloor :: Floor mx k -> Floor mx l -> Bool
aboveFloor x y = case compareFloor x y of GGT -> True; _ -> False

compareFloor :: forall l k mx. Floor mx k -> Floor mx l -> GOrdering k l
compareFloor MkFloor MkFloor = withSNat (snat @k) $ withSNat (snat @l) cmpNat

prevFloor' :: (GoodFloor f mx) => Floor ('S f) mx -> Floor f mx
prevFloor' MkFloor = MkFloor
