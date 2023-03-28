{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module ExpressionsSpec (
    spec,
  ) where

import TypeLevel.Expressions
import Test.Hspec

evaluate :: forall a. (SingI a, IsExp (Sing a) Integer) => Integer
evaluate = expEval (sing @a)

eql :: forall {k} (a :: k) (b :: k). (SDecide k, SingI a, SingI b) => Bool
eql = case (sing @a) %~ (sing @b) of
        Proved _ -> True
        _ -> False

spec :: SpecWith ()
spec = describe "Expressions" $ do
  describe "evaluation" $ do
    it "can evaluate numbers" $ do
      evaluate @1 `shouldBe` 1
      evaluate @10 `shouldBe` 10

    it "can apply binary operations" $ do
      evaluate @(1 ':+ (2 ':+ 3)) `shouldBe` 6
      evaluate @(1 ':- (2 ':- 3)) `shouldBe` 2
      evaluate @(1 ':* (2 ':* 3)) `shouldBe` 6
      evaluate @(1 ':+ (2 ':* (3 ':- 10))) `shouldBe` -13

    it "can apply unwary operations" $ do
      evaluate @('Neg 1) `shouldBe` -1
      evaluate @('Abs ('Neg 1)) `shouldBe` 1

  describe "equality" $ do
    it "can test equality of binary operations" $ do
      eql @(1 ':+ 1) @(1 ':+ 1) `shouldBe` True
      eql @(1 ':+ 1) @(2 ':+ 1) `shouldBe` False
      eql @(1 ':+ 1) @(1 ':+ 0) `shouldBe` False

      eql @(1 ':+ (1 ':- (1 ':* 1))) @(1 ':+ (1 ':- (1 ':* 1))) `shouldBe` True
      eql @(1 ':+ (1 ':- (1 ':* 1))) @(2 ':+ (1 ':- (1 ':* 1))) `shouldBe` False
      eql @(1 ':+ (1 ':- (1 ':* 1))) @(1 ':+ (2 ':- (1 ':* 1))) `shouldBe` False
      eql @(1 ':+ (1 ':- (1 ':* 1))) @(1 ':+ (1 ':- (2 ':* 1))) `shouldBe` False
      eql @(1 ':+ (1 ':- (1 ':* 1))) @(1 ':+ (1 ':- (1 ':* 2))) `shouldBe` False
