{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Elevator.Floor
  ( GoodFloor,
    BelowTop,
    Floor (..),
    SomeFloor (..),
    mkSomeFloor,
    prevFloor,
    nextFloor,
    belowFloor,
    aboveFloor,
    sameFloor,
    compareFloor,
    mkFloor,
    prevFloorBad,
    sFloor,
    withFloor,
    module X,
  )
where

import Data.GADT.Compare as X
import Data.Kind as X
import Data.Type.Dec
import Data.Type.Equality as X
import Data.Type.Nat as X
import Data.Type.Nat.LE as X
import Fmt

type GoodFloor :: Nat -> Nat -> Constraint
type GoodFloor mx f = (SNatI f, SNatI mx, LE f mx)

type BelowTop mx f = LE ('S f) mx

type Floor :: Nat -> Nat -> Type
data Floor mx f where
  MkFloor :: GoodFloor mx f => Floor mx f

type SomeFloor :: Nat -> Type
data SomeFloor mx where
  MkSomeFloor :: forall mx f. Floor mx f -> SomeFloor mx

mkSomeFloor :: forall mx. SNatI mx => Int -> Maybe (SomeFloor mx)
mkSomeFloor v
  | v < 0 = Nothing
  | otherwise = reify v' mk
  where
    v' = fromNatural $ fromIntegral v
    mk :: forall s p. SNatI s => p s -> Maybe (SomeFloor mx)
    mk _ = MkSomeFloor <$> (mkFloor @mx @s)

instance Show (Floor mx f) where
  show MkFloor = "Floor " +|| snatToNatural (snat @f) ||+ "/" +|| snatToNatural (snat @mx) ||+ ""

instance Show (SomeFloor mx) where
  show (MkSomeFloor e@MkFloor) = "SomeFloor (" +|| e ||+ ")"

instance Eq (Floor mx f) where
  x == y = show x == show y

sFloor :: Floor mx f -> (SNat mx, SNat f, LEProof f mx)
sFloor MkFloor = (snat, snat, leProof)

withFloor :: forall mx f r. Floor mx f -> (GoodFloor mx f => r) -> r
withFloor MkFloor f = f

mkFloor :: forall mx f. (SNatI mx, SNatI f) => Maybe (Floor mx f)
mkFloor = case decideLE @f @mx of
  Yes pf -> withLEProof pf $ Just MkFloor
  No _ -> Nothing

nextFloor :: forall mx f. BelowTop mx f => Floor mx f -> Floor mx ('S f)
nextFloor MkFloor = MkFloor @mx @('S f)

prevFloor :: forall mx f. Floor mx ('S f) -> Floor mx f
prevFloor MkFloor = withSNat floorPrf $ withLEProof (boundPrf @f) MkFloor
  where
    floorPrf :: SNat f
    floorPrf = case snat @('S f) of SS -> snat
    boundPrf :: forall f'. SNatI f' => LEProof f' mx
    boundPrf = case decideLE @f' @mx of
      Yes p -> p
      No _ -> undefined

-- BAD: not useful since caller will still have to prove (LE f mx)
prevFloorBad :: (GoodFloor mx f) => Floor mx ('S f) -> Floor mx f
prevFloorBad MkFloor = MkFloor

sameFloor :: Floor mx k -> Floor mx l -> Maybe (k :~: l)
sameFloor = testEquality

instance TestEquality (Floor mx) where
  testEquality x y = case compareFloor x y of GEQ -> Just Refl; _ -> Nothing

belowFloor :: Floor mx k -> Floor mx l -> Bool
belowFloor x y = case compareFloor x y of GLT -> True; _ -> False

aboveFloor :: Floor mx k -> Floor mx l -> Bool
aboveFloor x y = case compareFloor x y of GGT -> True; _ -> False

compareFloor :: forall l k mx. Floor mx k -> Floor mx l -> GOrdering k l
compareFloor MkFloor MkFloor = withSNat (snat @k) $ withSNat (snat @l) cmpNat
