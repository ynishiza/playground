{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module ElevatorSpec
  ( spec,
  )
where

import Data.List (isInfixOf)
import Data.Maybe
import Data.Type.Nat
import Elevator.Base
import GHC.IO.Exception
import Test.Hspec

fromFloor :: Floor mx f -> Elevator mx f 'Closed
fromFloor MkFloor = MkElevator

testIO :: ElevatorSystem a -> (a -> IO ()) -> IO ()
testIO e f = do
  (_, _, v) <- runElevatorSystem e (getState initialElevator)
  f v

mkLog :: Int -> DoorState -> ElevatorLog
mkLog f s = MkElevatorLog Nothing (MkElevatorState f s)

spec :: SpecWith ()
spec = describe "Elevator" $ do
  describe "Floor" $ do
    it "should get the next floor" $ do
      nextFloor bottomFloor `shouldBe` f1_10
      nextFloor f1_10 `shouldBe` f2_10
      nextFloor f3_10 `shouldBe` f4_10
      nextFloor f9_10 `shouldBe` topFloor

    it "should get previous floor" $ do
      prevFloor f1_10 `shouldBe` bottomFloor
      prevFloor topFloor `shouldBe` f9_10

    it "should get floor equal" $ do
      sameFloor bottomFloor bottomFloor `shouldSatisfy` isJust
      sameFloor f1_10 f1_10 `shouldSatisfy` isJust
      sameFloor f2_10 f2_10 `shouldSatisfy` isJust
      sameFloor topFloor topFloor `shouldSatisfy` isJust

    it "should get floor not equal" $ do
      sameFloor bottomFloor f1_10 `shouldNotSatisfy` isJust
      sameFloor bottomFloor topFloor `shouldNotSatisfy` isJust
      sameFloor f1_10 f2_10 `shouldNotSatisfy` isJust

    it "should make floor" $ do
      mkFloor @Mx @MyNum0 `shouldBe` Just bottomFloor
      mkFloor @Mx @MyNum1 `shouldBe` Just f1_10
      mkFloor @Mx @MyNum10 `shouldBe` Just topFloor
      mkFloor @MyNum1 @MyNum0 `shouldBe` Just (MkFloor @MyNum1 @MyNum0)

    it "should not make floor" $ do
      mkFloor @'Z @'Z `shouldSatisfy` isJust
      mkFloor @'Z @('S 'Z) `shouldSatisfy` isNothing

      mkFloor @Mx @Mx `shouldSatisfy` isJust
      mkFloor @Mx @('S Mx) `shouldSatisfy` isNothing

  describe "Elevator" $ do
    it "should move one floor up" $ do
      moveUp (fromFloor bottomFloor) `testIO` ((`shouldBe` f1_10) . getFloor)
      moveUp (fromFloor f9_10) `testIO` ((`shouldBe` topFloor) . getFloor)

    it "should move one floor down" $ do
      moveDown (fromFloor f1_10) `testIO` ((`shouldBe` bottomFloor) . getFloor)
      moveDown (fromFloor topFloor) `testIO` ((`shouldBe` f9_10) . getFloor)

    it "should move from bottom to top and vice versa" $ do
      moveTo bottomFloor (fromFloor topFloor) `testIO` ((`shouldBe` bottomFloor) . getFloor)
      moveTo topFloor (fromFloor bottomFloor) `testIO` ((`shouldBe` topFloor) . getFloor)

    it "should ensure open door" $ do
      ensureOpenedDoor (MkElevator @Mx @Mx @'Closed) `testIO` ((`shouldBe` Opened) . getDoorState)
      ensureOpenedDoor (MkElevator @Mx @Mx @'Opened) `testIO` ((`shouldBe` Opened) . getDoorState)

    it "should ensure closed door" $ do
      ensureClosedDoor (MkElevator @Mx @Mx @'Closed) `testIO` ((`shouldBe` Closed) . getDoorState)
      ensureClosedDoor (MkElevator @Mx @Mx @'Opened) `testIO` ((`shouldBe` Closed) . getDoorState)

  describe "Simulate" $ do
    it "runs a simulations" $ do
      simulate [MkSomeFloor (MkFloor @Mx @MyNum5), MkSomeFloor (MkFloor @Mx @MyNum3)]
        >>= ( `shouldBe`
                ( MkElevatorState 3 Opened,
                  [ mkLog 0 Closed,
                    mkLog 1 Closed,
                    mkLog 2 Closed,
                    mkLog 3 Closed,
                    mkLog 4 Closed,
                    mkLog 5 Closed,
                    mkLog 5 Opened,
                    mkLog 5 Closed,
                    mkLog 4 Closed,
                    mkLog 3 Closed,
                    mkLog 3 Opened
                  ]
                )
            )

    it "runs a simulations from test" $ do
      simulateFromText "4\n2"
        >>= ( `shouldBe`
                ( MkElevatorState 2 Opened,
                  [ mkLog 0 Closed,
                    mkLog 1 Closed,
                    mkLog 2 Closed,
                    mkLog 3 Closed,
                    mkLog 4 Closed,
                    mkLog 4 Opened,
                    mkLog 4 Closed,
                    mkLog 3 Closed,
                    mkLog 2 Closed,
                    mkLog 2 Opened
                  ]
                )
            )

    it "should throw an error if the input is not a number" $ do
      simulateFromText "a" `shouldThrow` (\(e :: IOException) -> "Failed to parse a" `isInfixOf` show e)

    it "should throw an error if the number is invalid" $ do
      simulateFromText "100" `shouldThrow` (\(e :: IOException) -> "Invalid floor number 100" `isInfixOf` show e)
      simulateFromText "-1" `shouldThrow` (\(e :: IOException) -> "Invalid floor number -1" `isInfixOf` show e)
