{-# LANGUAGE OverloadedStrings #-}
module Chapter14.StreamPlaySpec (
  spec
) where

import qualified Streaming.Prelude as S
import Chapter14.StreamPlay
import Test.Hspec

spec :: SpecWith ()
spec = describe "stream play" $ do
  describe "tabulate" $ do
    it "should tabulate" $  do
      tabulateCollapse 2 (S.each [1..10 :: Int]) >>= (`shouldBe` "1\t2\t\n3\t4\t\n5\t6\t\n7\t8\t\n9\t10\t\n")

    it "should tabulate and sum" $  do
      tabulateAndSum 2 (S.each [1..10 :: Int]) >>= (`shouldBe` ("1\t2\t\n3\t4\t\n5\t6\t\n7\t8\t\n9\t10\t\n", 55))
