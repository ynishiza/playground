module Spec
  ( spec,
  )
where

import Combinators
import qualified ParseBasic as P
import Test.Hspec
import Text.ParserCombinators.ReadP qualified as R

(===) :: (Show a, Eq a) => a -> a -> IO ()
x === y = shouldBe x y

spec :: Spec
spec = describe "read" $ do
  it "int" $ do
    let x = R.many (R.char '1')
    print $ R.readP_to_S x "1112"

    parse (P.integral @Int) "123" === [(1, "23"), (12, "3"), (123, "")]
    parse (P.integral @Int) "abc" === []
