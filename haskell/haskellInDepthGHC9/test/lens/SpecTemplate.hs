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

data MyData a m b = MyData
  { myProp1 :: String,
    myProp2 :: Int,
    -- variable
    myProp3 :: a,
    -- type constructor variable
    myProp4 :: m b
  }
  deriving stock (Eq, Show)

data MyGADTData a where
  MyGADTData :: {gProp1 :: String, gProp2 :: a} -> MyGADTData a
  deriving stock (Eq, Show)

data Tree a where
  Leaf :: a -> Tree a
  Node :: Tree a -> Tree a -> Tree a
  deriving stock (Eq, Show, Functor, Foldable, Traversable)

$(genFieldLenses ''MyData)
$(genFieldLenses ''MyGADTData)
$(genConstructorPrisms ''Tree)

spec :: Spec
spec = describe "Lens.Template" $ do
  it "Fields" $ do
    let x = MyData "hello" 1 True ['a']

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
      & view _myProp4
      & expects ['a']

    x
      & set _myProp1 "x"
      & expects (x { myProp1 = "x" })

    x
      & set _myProp2 2
      & expects (x { myProp2 = 2 })

    x
      & set _myProp3 False
      & expects (x { myProp3 = False })

    x
      & set (_myProp4 . traverse) '0'
      & expects (x { myProp4 = ['0'] })

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

  it "" $ do
    Leaf True
      & preview _Leaf
      & expects
      $ Just True
    Leaf True
      & preview _Node
      & expects Nothing
    Node (Leaf True) (Leaf False)
      & preview _Node
      & expects
      $ Just (Leaf True, Leaf False)
    Node (Leaf True) (Leaf False)
      & preview _Leaf
      & expects Nothing

    -- Leaf True
    --   & set _Leaf "a"
    --   & expects
    --   $ Leaf "a"
