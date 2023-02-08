module FreeSpec
  ( spec,
  )
where

import Data.Foldable
import Free
import Test.Hspec

spec :: SpecWith ()
spec = describe "F" $ do
  it "liftPure" $ do
    execListF (liftPure @Int @(List Int) 100) `shouldBe` ([], 100)

  it "liftF" $ do
    execListF (liftNil @Int) `shouldBe` ([], ())
    execListF (liftCons @Int 1) `shouldBe` ([1], ())

  it "binds" $ do
    execListF
      ( do
          liftCons (1 :: Int)
          liftCons 2
          liftCons 3
          liftCons 10
      )
      `shouldBe` ([1, 2, 3, 10], ())

  describe "cutoff" $ do
    it "should cutoff" $ do
      let l = liftList [1 .. 10 :: Int]
      traverse_ (\n -> execListF (cutoff n l) `shouldBe` (take n [1 .. 10], Nothing)) [0 .. 10]
