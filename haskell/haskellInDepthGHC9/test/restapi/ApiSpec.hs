module ApiSpec
  ( spec,
  )
where

import API3.API
import API3.BookAPI
import Data.Foldable
import Data.List (isInfixOf)
import Test.Hspec

spec :: SpecWith ()
spec = describe "API" $ do
  describe "book API" $ do
    let runRoute path cap = execAPIAction $ route bookAPIProxy bookAPI [path, cap]
        test res v = res >>= (`shouldBe` v)
        testError res h = res `shouldThrow` h

    it "should route" $ do
      test (runRoute "title" "1") "Haskell in depth"
      test (runRoute "year" "1") "2021"
      test (runRoute "rating" "1") "Good"

    it "should fail on non-existent routes" $ do
      let handler (RouteError _ msg) = "Route not found" `isInfixOf` msg
          handler _ = False
      traverse_
        (flip testError handler . flip runRoute "1")
        [ "a",
          "b",
          "c"
        ]

    it "should fail if the id fails to parse" $ do
      test (runRoute "title" "1") "Haskell in depth"
      testError (runRoute "title" "a") $
        \(CaptureError _ msg) -> "Failed to parse a" `isInfixOf` msg
      testError (runRoute "title" "") $
        \(CaptureError _ msg) -> "Failed to parse " `isInfixOf` msg
