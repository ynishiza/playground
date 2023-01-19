{-# LANGUAGE DataKinds #-}

module TypeLevel.CanonicalVectorsSpec
  ( spec,
  )
where

import Test.Hspec
import TypeLevel.CanonicalVectors

v0 :: Vec Integer 'Z
v0 = VNil

v1 :: Vec Integer ('S 'Z)
v1 = VCons 1 VNil

v2 :: Vec Integer ('S ('S ('S ('S 'Z))))
v2 = VCons 50 (VCons 30 (VCons 20 (VCons 10 VNil)))

shouldBeNonEmpty :: Vec a n -> Bool
shouldBeNonEmpty v = case vNull v of
  (Yes _) -> False
  (No _) -> True

spec :: SpecWith ()
spec = describe "Vec" $ do
  describe "comparison" $ do
    it "should check if vectors are equal" $ do
      (v0 `vEq` v0) `shouldBe` True
      (v1 `vEq` v1) `shouldBe` True
      (v2 `vEq` v2) `shouldBe` True

      (v0 `vEq` v1) `shouldBe` False
      (v0 `vEq` v2) `shouldBe` False
      (v1 `vEq` v0) `shouldBe` False
      (v1 `vEq` v2) `shouldBe` False
      (v2 `vEq` v0) `shouldBe` False
      (v2 `vEq` v1) `shouldBe` False

  describe "to/from list" $ do
    it "should convert to list" $ do
      vToList v0 `shouldBe` []
      vToList v1 `shouldBe` [1]
      vToList v2 `shouldBe` [50, 30, 20, 10]

    it "should convert from list" $ do
      vFromList [] `shouldSatisfy` \(MkSomeVec v) -> v0 `vEq` v
      vFromList [1] `shouldSatisfy` \(MkSomeVec v) -> v1 `vEq` v
      vFromList [50, 30, 20, 10] `shouldSatisfy` \(MkSomeVec v) -> v2 `vEq` v

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

  describe "vNotNull" $ do
    it "should check if non-empty" $ do
      shouldBeNonEmpty v0 `shouldBe` False
      shouldBeNonEmpty v1 `shouldBe` True
      shouldBeNonEmpty v2 `shouldBe` True

  describe "vAppend" $ do
    it "appends empty" $ do
      (v1 `vAppend` v0) `shouldBe` v1
      (v0 `vAppend` v1) `shouldBe` v1

    it "appends non-empty" $ do
      let v = VCons 1 (VCons 2 VNil)
      (v2 `vAppend` v) `shouldBe` VCons 50 (VCons 30 (VCons 20 (VCons 10 v)))
      (v `vAppend` v2) `shouldBe` VCons 1 (VCons 2 v2)

  describe "vCycle" $ do
    it "cycles 0 times" $ do
      vCycle v2 SZ `shouldBe` VNil
      vCycle v0 SZ `shouldBe` VNil

    it "cycles an empty vector" $ do
      vCycle @Int VNil (snat @Nat0) `shouldBe` VNil
      vCycle @Int VNil (snat @Nat1) `shouldBe` VNil
      vCycle @Int VNil (snat @Nat2) `shouldBe` VNil

    it "cycles"  $ do
      let v = VCons 2 (VCons 3 (VCons 1 VNil))
      vCycle @Integer v (snat @Nat1) `shouldBe` v
      vCycle @Integer v (snat @Nat2) `shouldBe` (v `vAppend` v)
      vCycle @Integer v (snat @Nat3) `shouldBe` (v `vAppend` (v `vAppend` v))
