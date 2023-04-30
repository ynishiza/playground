module Main (main) where

import qualified Property
import qualified Spec
import Test.Tasty
import Test.Tasty.Hedgehog
import Test.Tasty.Hspec

main :: IO ()
main = do
  hspecs <- testSpecs Spec.specs
  defaultMain $
    testGroup "isprime" $
      hspecs
        ++ [ fromGroup Property.properties
           ]
