{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}

module SpecTemplate (spec) where

import Data.Function
import Lens
import Lens.TH
import Test.Hspec

expects :: (Show a, Eq a) => a -> a -> Expectation
expects expected value = value `shouldBe` expected

data MyData a = MyData
  { myProp1 :: String,
    myProp2 :: Int,
    myProp3 :: a
  }
  deriving stock (Eq, Show)

data MyGADTData a where
  MyGADTData :: {gProp1 :: String, gProp2 :: a} -> MyGADTData a
  deriving stock (Eq, Show)

$(createFieldLenses ''MyData)
$(createFieldLenses ''MyGADTData)

spec :: Spec
spec = describe "Lens.Template" $ do
  it "Fields" $ do
    let x = MyData "hello" 1 True

    x
      & view _myProp1
      & expects "hello"

    x
      & view _myProp2
      & expects 1

    x
      & view _myProp3
      & expects True

    x
      & set _myProp1 "x"
      & expects (MyData "x" 1 True)

    x
      & set _myProp2 2
      & expects (MyData "hello" 2 True)

    x
      & set _myProp3 False
      & expects (MyData "hello" 1 False)

  it "GADT fields" $ do
    let x = MyGADTData "hello" True

    x
      & view _gProp1
      & expects "hello"

    x
      & view _gProp2
      & expects True

    x
      & set _gProp1 "x"
      & expects (MyGADTData "x" True)

    x
      & set _gProp2 False
      & expects (MyGADTData "hello" False)
