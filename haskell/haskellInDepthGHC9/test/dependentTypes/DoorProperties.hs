{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}

module DoorProperties
  ( group,
  )
where

import Data.Bifunctor
import Data.Maybe
import Door.Common (DoorState (..))
import Door.Door qualified as D
import Door.DoorGen qualified as DG
import Hedgehog
import Hedgehog.Gen

stateGen :: Gen DoorState
stateGen = enumBounded

doorGen :: Gen (D.SomeDoor, DG.SomeDoor)
doorGen = f . show <$> stateGen
  where
    f x = (fromJust $ D.parseDoor x, fromJust $ DG.parseDoor x)

doorEq :: (MonadTest m, D.SDoorStateI s1, DG.SingI s2) => (D.Door s1, DG.Door s2) -> m ()
doorEq (d1, d2) = D.doorState d1 === DG.doorState d2

usePair :: forall a. (forall s t. (D.SDoorStateI s, DG.SingI t) => (D.Door s, DG.Door t) -> a) -> (D.SomeDoor, DG.SomeDoor) -> a
usePair f (d1, d2) = D.withSomeDoor g d1
  where
    g :: forall s'. D.SDoorStateI s' => D.Door s' -> a
    g d1' = DG.withSomeDoorI (f @s' . (d1',)) d2

group :: Group
group =
  Group
    { groupName = "Door implementation equality",
      groupProperties =
        [ ( "Should be the same doors",
            property $ do
              doors <- forAll doorGen
              usePair doorEq doors
              pure ()
          ),
          ( "Should toggle both",
            property $ do
              doors <- forAll doorGen
              usePair doorEq $
                usePair (bimap D.toggleState DG.toggleState) doors
          )
        ]
    }
