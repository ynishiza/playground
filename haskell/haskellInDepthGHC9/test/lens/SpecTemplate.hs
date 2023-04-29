{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}

module SpecTemplate (spec) where

import Data.Function
import Language.Haskell.TH.Syntax
import Lens
import Lens.TH
import Lens.TH.Utils
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

-- case: monomorphic field lenses
-- since fields have overlapping type variables 
data Point a where
  Point :: {coordX :: a, coordY :: a} -> Point a
  deriving stock (Eq, Show)

data Tree a = Leaf a | Node (Tree a) (Tree a)
  deriving stock (Eq, Show, Functor, Foldable, Traversable)

data GADTTree a where
  GADTLeaf :: a -> GADTTree a
  GADTNode :: GADTTree a -> GADTTree a -> GADTTree a
  deriving stock (Eq, Show, Functor, Foldable, Traversable)

$(genFieldLenses ''MyData)
$(genFieldLenses ''MyGADTData)
$(genConstructorPrisms ''Tree)
$(genConstructorPrisms ''GADTTree)
$(genFieldLenses ''Point)

spec :: Spec
spec = describe "Lens.Template" $ do
  it "hasVariableOverlapWith" $ do
    let i = ConT $ mkName "Int"
        x = VarT $ mkName "x"
        y = VarT $ mkName "y"
    i
      `hasVariableOverlap` i
      & expects False
    i
      `hasVariableOverlap` x
      & expects False
    x
      `hasVariableOverlap` x
      & expects True
    y
      `hasVariableOverlap` x
      & expects False
    x
      `hasVariableOverlapWith` [i, y, i, y]
      & expects False
    x
      `hasVariableOverlapWith` [x, y, i, x]
      & expects True

  describe "Record field Optics" $ do
    it "Field optics" $ do
      let x = MyData "hello" 1 True ['a']

      -- get
      x
        & view _myProp1
        & expects "hello"

      -- set
      x
        & set _myProp1 "x"
        & expects (x {myProp1 = "x"})

    it "Getter" $ do
      let x = MyData "hello" 1 True ['a']
      x
        & view _myProp2
        & expects 1

      x
        & view _myProp3
        & expects True
      x
        & view _myProp4
        & expects ['a']

    it "Setter" $ do
      let x = MyData "hello" 1 True ['a']
      -- set
      x
        & set _myProp1 "x"
        & expects (x {myProp1 = "x"})

      x
        & set _myProp2 2
        & expects (x {myProp2 = 2})

      x
        & set _myProp3 False
        & expects (x {myProp3 = False})

      x
        & set (_myProp4 . traverse) '0'
        & expects (x {myProp4 = ['0']})

    it "GADT field optics" $ do
      let x = MyGADTData "hello" True

      -- get
      x
        & view _gProp1
        & expects "hello"

      x
        & view _gProp2
        & expects True

      -- set
      x
        & set _gProp1 "x"
        & expects (MyGADTData "x" True)

      x
        & set _gProp2 False
        & expects (MyGADTData "hello" False)

    describe "polymorphism" $ do
      it "may be polymorphic if there are is no variable overlap with other fields" $ do
        -- polymorphic
        MyData "hello" 1 True ['a']
          & set _myProp3 ()
          & expects (MyData "hello" 1 () ['a'])
        MyData "hello" 1 True ['a']
          & set _myProp4 (Just False)
          & expects (MyData "hello" 1 True (Just False))

      it "is monomorphic if there is an overlap" $ do
        Point (1 :: Int) 0
          & set _coordX 10
          & expects (Point 10 0)
        Point (1 :: Int) 0
          & set _coordY 10
          & expects (Point 1 10)

  describe "Sum type prism" $ do
    it "sum type prism" $ do
      Leaf True
        & preview _Leaf
        & expects
        $ Just True

      Leaf True
        & preview _Node
        & expects
        $ Nothing

      Node (Leaf True) (Leaf False)
        & preview _Node
        & expects
        $ Just (Leaf True, Leaf False)

      Node (Leaf True) (Leaf False)
        & preview _Leaf
        & expects Nothing

    it "GADT sum type prism" $ do
      GADTLeaf True
        & preview _GADTLeaf
        & expects (Just True)

      GADTLeaf True
        & preview _GADTNode
        & expects Nothing

      GADTNode (GADTLeaf True) (GADTLeaf False)
        & preview _GADTNode
        & expects
        $ Just (GADTLeaf True, GADTLeaf False)
      GADTNode (GADTLeaf True) (GADTLeaf False)
        & preview _GADTLeaf
        & expects Nothing

-- GADTLeaf True
--   & set _GADTLeaf "a"
--   & expects
--   $ GADTLeaf "a"
