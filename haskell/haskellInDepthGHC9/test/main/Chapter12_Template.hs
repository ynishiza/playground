{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Chapter12_Template (specs) where

import Chapter12.Base
import Chapter12.TemplateProjection
import Control.Exception
import Data.Tuple
import Fmt
import Test.Hspec

specs :: SpecWith ()
specs = describe "templates" $ do
  let t1 :: Solo Int
      t1 = Solo 1
      t2 :: (Int, Int)
      t2 = (1, 2)
      t3 :: (Int, Int, Int)
      t3 = (1, 2, 3)
      t4 :: (Int, Int, Int, Int)
      t4 = (1, 2, 3, 4)

  describe "proj_n_k" $ do
    describe "Q Exp" $ do
      it "should project" $ do
        $(project_rawAst 3 0) t3 `shouldBe` 1
        $(project_rawAst 3 1) t3 `shouldBe` 2
        $(project_rawAst 3 2) t3 `shouldBe` 3

      it "project_rawAst vs project_withExpr" $ do
        let t :: (Int, Int, Int)
            t = (1, 2, 3)
        $(project_rawAst 3 0) t `shouldBe` $(project_withExpr 3 0) t
        $(project_rawAst 3 1) t `shouldBe` $(project_withExpr 3 1) t
        $(project_rawAst 3 2) t `shouldBe` $(project_withExpr 3 2) t

    it "should project" $ do
      proj_1_0 t1 `shouldBe` 1
      proj_2_0 t2 `shouldBe` 1
      proj_2_1 t2 `shouldBe` 2
      proj_3_0 t3 `shouldBe` 1
      proj_3_1 t3 `shouldBe` 2
      proj_3_2 t3 `shouldBe` 3

  it "t" $ do
    $(toTupleE 1) [1] `shouldBe` t1
    $(toTupleE 2) [1, 2] `shouldBe` t2
    $(toTupleE 3) [1, 2, 3] `shouldBe` t3
    $(toTupleE 4) [1, 2, 3, 4] `shouldBe` t4

  describe "toTuple" $ do
    describe "Q Exp" $ do
      it "toTuple_n" $ do
        toTuple_1 [1] `shouldBe` t1
        toTuple_2 [1, 2] `shouldBe` t2
        toTuple_3 [1, 2, 3] `shouldBe` t3
        toTuple_4 [1, 2, 3, 4] `shouldBe` t4

    it "should convert to tuple" $ do
      toTuple_1 [1] `shouldBe` t1
      toTuple_2 [1, 2] `shouldBe` t2
      toTuple_3 [1, 2, 3] `shouldBe` t3
      toTuple_4 [1, 2, 3, 4] `shouldBe` t4

    it "should throw an error on length mismatch" $ do
      evaluate (toTuple_1 []) `shouldThrow` f 1
      evaluate (toTuple_2 []) `shouldThrow` f 2
      evaluate (toTuple_3 []) `shouldThrow` f 3
        where
          f :: Int -> SomeException -> Bool
          f n (SomeException e) = msg == ("List must be exactly length " +| n |+ "")
            where
              msg = head $ lines $ show e
