{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Chapter12_Template (specs) where

import Chapter12.TemplateProjection
import Chapter12.Base
import Test.Hspec

specs :: SpecWith ()
specs = describe "templates" $ do
  let 
      t2 :: (Int, Int)
      t2 = (1, 2)
      t3 :: (Int, Int, Int)
      t3 = (1, 2, 3)
  describe "project tuple" $ do
    describe "projection quote" $ do
      it "should project triple" $ do
        $(project_rawAst 3 0) t3 `shouldBe` 1
        $(project_rawAst 3 1) t3 `shouldBe` 2
        $(project_rawAst 3 2) t3 `shouldBe` 3

      it "project_rawAst vs project_withExpr" $ do
        let t :: (Int, Int, Int)
            t = (1, 2, 3)
        $(project_rawAst 3 0) t `shouldBe` $(project_withExpr 3 0) t
        $(project_rawAst 3 1) t `shouldBe` $(project_withExpr 3 1) t
        $(project_rawAst 3 2) t `shouldBe` $(project_withExpr 3 2) t

  describe "proj_n_k" $ do
    it "proj_n_k" $ do
      proj_2_0 t2 `shouldBe` 1
      proj_2_1 t2 `shouldBe` 2
      proj_3_0 t3 `shouldBe` 1
      proj_3_1 t3 `shouldBe` 2
      proj_3_2 t3 `shouldBe` 3
