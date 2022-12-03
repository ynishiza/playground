{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Chapter12_Template (specs) where

import Chapter12.TemplateProjection
import Test.Hspec

specs :: SpecWith ()
specs = describe "" $ do
  it "should project triple" $ do
    let t :: (Int, Int, Int)
        t = (1, 2, 3)
    $(project 3 0) t `shouldBe` 1
    $(project 3 1) t `shouldBe` 1
    $(project 3 2) t `shouldBe` 1

-- a `shouldBe` 2
