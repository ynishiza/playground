{-# LANGUAGE DataKinds #-}

module TypeLevel.CanonicalVectorsSpec
  ( spec,
  )
where

import Test.Hspec
import TypeLevel.CanonicalVectors

v0 :: Vec a 'Z
v0 = VNil

v1 :: Vec Integer ('S 'Z)
v1 = VCons 1 VNil

v2 :: Vec Integer ('S ('S ('S ('S 'Z))))
v2 = VCons 50 (VCons 30 (VCons 20 (VCons 10 VNil)))

spec :: SpecWith ()
spec = describe "Vec" $ do
  describe "vLength" $ do
    it "should get the length" $ do
      vLength v0 `shouldBe` 0
      vLength v1 `shouldBe` 1
      vLength v2 `shouldBe` 4

  describe "vHead" $ do
    it "should get the head element of a non-empty vector" $ do
      vHead v1 `shouldBe` 1
      vHead v2 `shouldBe` 50

  describe "vElem" $ do
    it "can select first item if there is at least one element" $ do
      v1 `vElem` (snat @Nat0) `shouldBe` 1

    it "can select any element" $ do
      v2 `vElem` (snat @Nat0) `shouldBe` 50
      v2 `vElem` (snat @Nat1) `shouldBe` 30
      v2 `vElem` (snat @Nat2) `shouldBe` 20
      v2 `vElem` (snat @Nat3) `shouldBe` 10
