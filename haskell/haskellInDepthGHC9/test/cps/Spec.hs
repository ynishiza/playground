module Spec (spec) where

import CPS.List
import CPS.Reader
import CPS.Writer
import CPS.State
import Data.Foldable
import Data.Function
import System.IO.Extra
import Test.Hspec

spec :: Spec
spec = describe "" $ do
  describe "CList" $ do
    it "is a list" $ do
      let list = 1 |: 2 |: 3 |: nil

      runCList @Int nil [] (:) `shouldBe` []
      runCList @Int list [] (:) `shouldBe` [1, 2, 3]

    it "is traversable" $ do
      let list = to @CList [1 :: Int, 2, 3]
      (s, _) <- captureOutput $ traverse_ print list
      s `shouldBe` "1\n2\n3\n"

  describe "CWriter" $ do
    it "can write" $ do
      let wrt = do
            tell "Hello "
            tell "World"
            return 1
      runCWriter wrt id `shouldBe` (1 :: Int, "Hello World")

    it "can listen" $ do
      let wrt = do
            tell "Hello "
            tell "World"
            return 1
      ( runCWriter (listen wrt) id
          & fst
        )
        `shouldBe` (1 :: Int, "Hello World")

  describe "CReader" $ do
    it "can read" $ do
      let rd = do
            x <- asks (* 2)
            asks (+ x)
      runCReader rd 10 id `shouldBe` (30 :: Int)

  describe "CState" $ do
    it "can hold state" $ do
      let st = do
            put 1
            modify (+10)
            x <- get
            return $  x * 100
      runCState st id 0 `shouldBe` (1100, 11 :: Int)

