{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Chapter12_Template (specs) where

-- import Language.Haskell.TH
import Chapter12.TemplateProjection
import Test.Tasty.Hspec

x = $(project 3 1) (1,2,3)

specs = describe "" $ do
  it "" $ do
    2 `shouldBe` 1
    a `shouldBe` 2
