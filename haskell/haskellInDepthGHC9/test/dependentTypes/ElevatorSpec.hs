{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}

module ElevatorSpec
  ( MyNum0,
    MyNum1,
    MyNum6,
    MaxFloor,
    f0,
    f1,
    f2,
    f3,
    f4,
    f5,
    f6,
    spec,
  )
where

import Data.Maybe
import Data.Type.Nat
import Test.Hspec
import Elevator.TH

$(genFloors 6)

spec :: SpecWith ()
spec = describe "Elevator" $ do
  it "should get the next floor" $ do
    nextFloor f0 `shouldBe` f1
    nextFloor f1 `shouldBe` f2
    nextFloor f3 `shouldBe` f4
    nextFloor f5 `shouldBe` f6

  it "should get previous floor" $ do
    prevFloor f1 `shouldBe` f0
    prevFloor f6 `shouldBe` f5

  it "should get floor equal" $ do
    sameFloor f0 f0 `shouldSatisfy` isJust
    sameFloor f1 f1 `shouldSatisfy` isJust
    sameFloor f2 f2 `shouldSatisfy` isJust
    sameFloor f6 f6 `shouldSatisfy` isJust

  it "should get floor not equal" $ do
    sameFloor f0 f1 `shouldNotSatisfy` isJust
    sameFloor f0 f6 `shouldNotSatisfy` isJust
    sameFloor f1 f2 `shouldNotSatisfy` isJust

  it "should make floor" $ do
    mkFloor @MaxFloor @MyNum0 `shouldBe` Just f0
    mkFloor @MaxFloor @MyNum1 `shouldBe` Just f1
    mkFloor @MaxFloor @MyNum6 `shouldBe` Just f6
    mkFloor @MyNum1 @MyNum0 `shouldBe` Just (MkFloor @MyNum1 @MyNum0)

  it "should not make floor" $ do
    mkFloor @'Z @'Z `shouldSatisfy` isJust
    mkFloor @'Z @('S 'Z) `shouldSatisfy` isNothing

    mkFloor @MaxFloor @MaxFloor `shouldSatisfy` isJust
    mkFloor @MaxFloor @('S MaxFloor) `shouldSatisfy` isNothing
