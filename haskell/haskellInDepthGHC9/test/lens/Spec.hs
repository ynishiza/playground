{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Take on a non-positive" #-}
module Spec
  ( spec,
  )
where

import Control.Monad.State
import Data.Char
import Data.Function
import Lens
import System.IO.Extra
import Test.Hspec

data Alpha = NotAlpha | A | B | C | D | E | F | G | H | I | J | K | L | M | N | O | P | Q | R | S | T | U | V | W | X | Y | Z
  deriving (Eq, Ord, Show, Bounded, Enum)

instance Semigroup Alpha where
  a <> b = toEnum $ max (fromEnum a) (fromEnum b)

instance Monoid Alpha where
  mempty = NotAlpha

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

  describe "Lens.Get" $ do
    it "gets" $ do
      let x =
            ( A,
              ( B,
                ( ['a' .. 'e'],
                  (1 :: Int, True)
                )
              )
            )
      x & view _1 & expects A
      x & view (_2 . _1) & expects B
      x & view (_2 . _2 . _1) & expects "abcde"
      x & toListOf (_2 . _2 . _1 . folded . to toUpper) & expects "ABCDE"

      x & views _1 show & expects "A"
      x & views (_2 . _1) show & expects "B"

      let s = liftIO $ print "Hello"
      (s >> use _1) & flip evalStateT x & expectsIO A
      (s >> use (_2 . _1)) & flip evalStateT x & expectsIO B
      (s >> uses _1 succ) & flip evalStateT x & expectsIO B

    it "combinators" $ do
      let x =
            ( A,
              ( B,
                ( ['a' .. 'e'],
                  (1 :: Int, True)
                )
              )
            )

      -- like: constant
      x & view (_1 . like 'a') & expects 'a'
      x & view (_2 . like 'a') & expects 'a'
      x & toListOf (_2 . _2 . _1 . folded . like 'a') & expects $ take 5 ['a', 'a' ..]

      x & view (_1 . to succ) & expects B
      x & view (_2 . _1 . to succ) & expects C
      x & toListOf (_2 . _2 . _1 . folded . to toUpper) & expects "ABCDE"

  describe "Lens.Fold" $ do
    it "folds" $ do
      zip [A .. E] ['a' ..] & foldOf (folded . _1) & expects E
      zip [A .. E] ['a' ..] & preview (folded . _1) & expects $ Just A
      zip [A .. E] [] & preview (folded . _1) & expects Nothing

      zip [A .. E] ['a' ..] & toListOf (folded . _1) & expects [A .. E]
      zip [A .. E] ['a' ..] & toListOf (folded . _2) & expects "abcde"

    it "Combinators" $ do
      (A, B) & toListOf (_1 . replicated 3) & expects [A, A, A]
      (A, B) & toListOf (_1 . repeated) & take 0 & expects []
      (A, B) & toListOf (_1 . repeated) & take 5 & expects [A, A, A, A, A]
      (A, B) & toListOf (_1 . iterated (succ . succ)) & take 5 & expects [A, C, E, G, I]

      [A .. E] & toListOf (folded . filtered (> C)) & expects [D, E]
      (A, B) & view (_1 . filtered (> A)) & expects mempty
      (A, B) & view (_2 . filtered (> A)) & expects B

      [A .. E] & toListOf (backwards folded) & expects [E, D, C, B, A]
      (A, B) & view (backwards _1) & expects A

      ([A .. E], []) & has (_1 . folded) & expects True
      ([A .. E], []) & has (_2 . folded) & expects False

      -- taking/dropping
      (A, B) & preview (taking (_1 . replicated 10) 3) & expects $ Just A
      (A, B) & toListOf (taking (_1 . replicated 10) 3) & expects [A, A, A]
      -- infinite lists
      (A, B) & preview (_1 . repeated) & expects $ Just A
      (A, B) & toListOf (taking (_1 . repeated) 3) & expects [A, A, A]
      (A, B) & toListOf (taking (_1 . repeated) 5) & expects [A, A, A, A, A]
      (A, B) & toListOf (takingWhile (_1 . iterated succ) (< E)) & expects [A, B, C, D]
      (A, B) & toListOf (takingWhile (_1 . iterated succ) (< E)) & expects [A, B, C, D]

      let text =
            "hello world\n\
            \a\n \
            \b\n \
            \"
      text & toListOf lined & expects $ lines text
      text & toListOf worded & expects $ words text

    it "[Prelude]" $ do
      let x =
            [A .. E]
              & flip zip [1 :: Int ..]

      x & toIndexedList (folded . _1) & expects $ [(0, A), (1, B), (2, C), (3, D), (4, E)]

      x & foldrOf (folded . _1) (:) [] & expects $ foldr (:) [] [A .. E]
      x & foldlOf (folded . _1) (flip (:)) [] & expects $ foldl (flip (:)) [] [A .. E]
      x & maxOf (folded . _1) & expects E
      [] & maxOf folded & expects NotAlpha
      x & maxOf' (folded . _1) & expects $ Just E
      [] & maxOf' folded & expects @(Maybe Alpha) Nothing

      x & elemOf (folded . _1) C & expects True
      x & elemOf (folded . _1) T & expects False
      A & nullOf repeated & expects False
      A & notNullOf repeated & expects True
      x & firstOf (folded . _1) & expects $ Just A
      x & firstOf (folded . _2) & expects $ Just 1
      x & lastOf (folded . _1) & expects $ Just E
      x & lastOf (folded . _2) & expects $ Just 5
