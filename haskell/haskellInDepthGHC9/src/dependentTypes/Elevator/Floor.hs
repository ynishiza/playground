module Elevator.Floor
  ( GoodFloor,
    BelowTop,
    Floor (..),
    prevFloor,
    nextFloor,
    sameFloor,
    belowFloor,
    aboveFloor,
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

instance Show (Floor mx f) where
  show MkFloor = "Floor " +|| snat @mx ||+ " " +|| snat @f ||+ ""

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
prevFloor MkFloor = withLEProof boundPrf $ withSNat floorPrf MkFloor
  where
    floorPrf :: SNat f
    floorPrf = case snat @('S f) of SS -> snat
    boundPrf :: LEProof f mx
    boundPrf = leStepL (leProof @('S f) @mx)

-- BAD: not useful since caller will still have to prove (LE f mx)
prevFloorBad :: (GoodFloor mx f) => Floor mx ('S f) -> Floor mx f
prevFloorBad MkFloor = MkFloor

sameFloor :: Floor mx k -> Floor mx l -> Maybe (k :~: l)
sameFloor x y = case compareFloor x y of GEQ -> Just Refl; _ -> Nothing

belowFloor :: Floor mx k -> Floor mx l -> Bool
belowFloor x y = case compareFloor x y of GLT -> True; _ -> False

aboveFloor :: Floor mx k -> Floor mx l -> Bool
aboveFloor x y = case compareFloor x y of GGT -> True; _ -> False

compareFloor :: forall l k mx. Floor mx k -> Floor mx l -> GOrdering k l
compareFloor MkFloor MkFloor = withSNat (snat @k) $ withSNat (snat @l) cmpNat
