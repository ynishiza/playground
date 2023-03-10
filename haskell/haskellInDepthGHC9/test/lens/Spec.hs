module Spec
  ( spec,
  )
where

import Data.Function
import Lens
import System.IO.Extra
import Test.Hspec

data Alpha = NA | A | B | C | D | E | F | G | H | I | J | K | L | M | N | O | P | Q | R | S | T | U | V | W | X | Y | Z
  deriving (Eq, Ord, Show, Bounded, Enum)

instance Semigroup Alpha where
  a <> b = toEnum $ max (fromEnum a) (fromEnum b)

instance Monoid Alpha where
  mempty = NA

expects :: (Show a, Eq a) => a -> a -> Expectation
expects = flip shouldBe

expectsIO :: (Show a, Eq a) => a -> IO a -> Expectation
expectsIO expected io = io >>= (`shouldBe` expected)

expectsIOCapture :: String -> IO () -> Expectation
expectsIOCapture expected io = captureOutput io >>= (`shouldBe` (expected, ()))

spec :: Spec
spec = describe "" $ do
  it "" $ do
    (A, True) & view _1 & expects A
    (A, True) & view _2 & expects True
  -- zip [A .. E] ['a'..] & toListOf (folded . _2) & expects []
  --

  describe "Fold" $ do
    it "folds" $ do
      zip [A .. E] ['a' ..] & foldOf (folded . _1) & expects E
      zip [A .. E] ['a' ..] & preview (folded . _1) & expects $ Just A
      zip [A .. E] [] & preview (folded . _1) & expects Nothing

      zip [A .. E] ['a' ..] & toListOf (folded . _1) & expects [A .. E]
      zip [A .. E] ['a' ..] & toListOf (folded . _2) & expects "abcde"

    it "[Prelude]" $ do
      let x =
            [A .. E]
              & flip zip [1 :: Int ..]
      x & maxOf (folded . _1) & expects E
      [] & maxOf folded & expects NA
      x & maxOf' (folded . _1) & expects $ Just E
      [] & maxOf' folded & expects @(Maybe Alpha) Nothing

      x & elemOf (folded . _1) C & expects True
      x & elemOf (folded . _1) T & expects False
