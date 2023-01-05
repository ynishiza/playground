{-# LANGUAGE TemplateHaskell #-}

module ElevatorSpec
  ( MyNum0,
    MyNum1,
    MyNum6,
    MaxFloor_6,
    spec,
  )
where

import Data.Maybe
import Data.Type.Nat
import Elevator.Elevator
import Elevator.FloorTH
import Test.Hspec

$(genNumAliases 0 6)
$(genFloors 6)

fromFloor :: Floor mx f -> Elevator mx f 'Closed
fromFloor MkFloor = MkElevator

spec :: SpecWith ()
spec = describe "Elevator" $ do
  describe "Floor" $ do
    it "should get the next floor" $ do
      nextFloor f0_6 `shouldBe` f1_6
      nextFloor f1_6 `shouldBe` f2_6
      nextFloor f3_6 `shouldBe` f4_6
      nextFloor f5_6 `shouldBe` f6_6

    it "should get previous floor" $ do
      prevFloor f1_6 `shouldBe` f0_6
      prevFloor f6_6 `shouldBe` f5_6

    it "should get floor equal" $ do
      sameFloor f0_6 f0_6 `shouldSatisfy` isJust
      sameFloor f1_6 f1_6 `shouldSatisfy` isJust
      sameFloor f2_6 f2_6 `shouldSatisfy` isJust
      sameFloor f6_6 f6_6 `shouldSatisfy` isJust

    it "should get floor not equal" $ do
      sameFloor f0_6 f1_6 `shouldNotSatisfy` isJust
      sameFloor f0_6 f6_6 `shouldNotSatisfy` isJust
      sameFloor f1_6 f2_6 `shouldNotSatisfy` isJust

    it "should make floor" $ do
      mkFloor @MaxFloor_6 @MyNum0 `shouldBe` Just f0_6
      mkFloor @MaxFloor_6 @MyNum1 `shouldBe` Just f1_6
      mkFloor @MaxFloor_6 @MyNum6 `shouldBe` Just f6_6
      mkFloor @MyNum1 @MyNum0 `shouldBe` Just (MkFloor @MyNum1 @MyNum0)

    it "should not make floor" $ do
      mkFloor @'Z @'Z `shouldSatisfy` isJust
      mkFloor @'Z @('S 'Z) `shouldSatisfy` isNothing

      mkFloor @MaxFloor_6 @MaxFloor_6 `shouldSatisfy` isJust
      mkFloor @MaxFloor_6 @('S MaxFloor_6) `shouldSatisfy` isNothing

  describe "Elevator" $ do
    it "should move up" $ do
      moveUp (fromFloor f0_6) >>= ((`shouldBe` f1_6) . getFloor)
      moveUp (fromFloor f5_6) >>= ((`shouldBe` f6_6) . getFloor)

    it "should move down" $ do
      moveDown (fromFloor f1_6) >>= ((`shouldBe` f0_6) . getFloor)
      moveDown (fromFloor f6_6) >>= ((`shouldBe` f5_6) . getFloor)
