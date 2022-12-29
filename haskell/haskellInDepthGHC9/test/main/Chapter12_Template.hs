{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Chapter12_Template (specs) where

import Chapter12.Base
import Chapter12.TemplateProjection
import Chapter12.TemplateReify
import Control.Exception
import Control.Monad
import Data.Foldable
import Data.Tuple
import Fmt
import System.Random
import Test.Hspec

data TestData a = TestData
  { v1 :: a,
    v2 :: a,
    v3 :: a,
    v4 :: a,
    p1 :: Solo a,
    p2 :: (a, a),
    p3 :: (a, a, a),
    p4 :: (a, a, a, a)
  }

setup :: IO (TestData Int)
setup = do
  [i1, i2, i3, i4] <- replicateM 4 $ getStdRandom uniform
  return $
    TestData
      i1
      i2
      i3
      i4
      (Solo i1)
      (i1, i2)
      (i1, i2, i3)
      (i1, i2, i3, i4)

specs :: SpecWith ()
specs =
  setup
    `before` describe
      "templates"
      ( do
          describe "proj_n_k" $ do
            describe "Q Exp" $ do
              it "should project" $ \TestData {..} -> do
                $(project_rawAst 3 0) p3 `shouldBe` v1
                $(project_rawAst 3 1) p3 `shouldBe` v2
                $(project_rawAst 3 2) p3 `shouldBe` v3

              it "project_rawAst vs project_withExpr" $ \TestData {..} -> do
                $(project_rawAst 3 0) p3 `shouldBe` $(project_withExpr 3 0) p3
                $(project_rawAst 3 1) p3 `shouldBe` $(project_withExpr 3 1) p3
                $(project_rawAst 3 2) p3 `shouldBe` $(project_withExpr 3 2) p3

            it "should project" $ \TestData {..} -> do
              proj_1_0 p1 `shouldBe` v1
              proj_2_0 p2 `shouldBe` v1
              proj_2_1 p2 `shouldBe` v2
              proj_3_0 p3 `shouldBe` v1
              proj_3_1 p3 `shouldBe` v2
              proj_3_2 p3 `shouldBe` v3

          describe "reification" $ do
            it "should identify shapes" $ \_ -> do
              let testPredicate p expected = traverse_ (\x -> p x `shouldBe` expected == x) [Circle, Square, Triangle]
              testPredicate isCircle Circle
              testPredicate isSquare Square
              testPredicate isTriangle Triangle

          describe "listToTuple" $ do
            it "should convert to tuple" $ \TestData {..} -> do
              listToTuple_1 [v1] `shouldBe` p1
              listToTuple_2 [v1, v2] `shouldBe` p2
              listToTuple_3 [v1, v2, v3] `shouldBe` p3
              listToTuple_4 [v1, v2, v3, v4] `shouldBe` p4

            it "should throw an error on length mismatch" $ \_ -> do
              let f :: Int -> MyTemplateError -> Bool
                  f n (MkMyTemplateError msg) = msg == ("List must be exactly length " +| n |+ "")
              evaluate (listToTuple_1 []) `shouldThrow` f 1
              evaluate (listToTuple_2 []) `shouldThrow` f 2
              evaluate (listToTuple_3 []) `shouldThrow` f 3
      )
