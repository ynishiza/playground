{-# LANGUAGE DataKinds #-}

module DoorSpec
  ( spec,
  )
where

import Data.Foldable
import Door.Common (DoorState (..))
import Door.Common qualified as DC
import Door.Door qualified as D
import Door.DoorGen qualified as DG
import Test.Hspec
import Data.Singletons

eq :: D.SomeDoor -> DG.SomeDoor -> Expectation
eq (D.MkSomeDoor d1) (DG.MkSomeDoor d2) = D.doorState d1 `shouldBe` DG.doorState d2

spec = describe "Door" $ do
  let dro = D.mkDoor D.SOpen
      drc = D.mkDoor D.SClosed
      drgo = DG.mkDoor DG.SOpen
      drgc = DG.mkDoor DG.SClosed
  it "should agree" $ do
    D.doorState dro `shouldBe` DG.doorState drgo
    D.doorState drc `shouldBe` DG.doorState drgc
    D.MkSomeDoor (D.openAnyDoor dro) `eq` DG.MkSomeDoor (DG.openAnyDoor drgo)
    D.MkSomeDoor (D.openAnyDoor drc) `eq` DG.MkSomeDoor (DG.openAnyDoor drgc)
    D.toggleState dro `eq` DG.toggleState drgo
    D.toggleState drc `eq` DG.toggleState drgc

  it "should open any door" $ do
    traverse_
      (`shouldBe` Open)
      [ D.doorState $ D.openAnyDoor dro,
        D.doorState $ D.openAnyDoor drc,
        DG.doorState $ DG.openAnyDoor drgo,
        DG.doorState $ DG.openAnyDoor drgc
      ]
