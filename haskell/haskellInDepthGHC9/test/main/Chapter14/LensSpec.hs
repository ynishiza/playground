module Chapter14.LensSpec
  ( spec,
  )
where

import Control.Lens
import Control.Monad.Trans.State
import Control.Monad.Trans.Writer
import Data.Char
import Data.Monoid
import Test.Hspec

expects :: (Show a, Eq a) => a -> a -> Expectation
expects = flip shouldBe

timesN :: (Profunctor p, Contravariant f, Num a) => a -> Optic' p f a a
timesN n = to (* n)

addN :: (Profunctor p, Contravariant f, Num a) => a -> Optic' p f a a
addN n = to (+ n)

spec :: Spec
spec = describe "Lens" $ do
  describe "Getter" $ do
    it "Basic" $ do
      (1 :: Int, 2 :: Int) & view _1 & expects 1
      (1 :: Int, 2 :: Int) & view _2 & expects 2
      (1 :: Int, 2 :: Int) & views _2 show & expects "2"

      -- compose
      ((1 :: Int, 2 :: Int), ("A", True)) & view (_1 . _1) & expects 1
      ((1 :: Int, 2 :: Int), ("A", True)) & view (_1 . _2) & expects 2
      ((1 :: Int, 2 :: Int), ("A", True)) & view (_2 . _1) & expects "A"
      ((1 :: Int, 2 :: Int), ("A", True)) & view (_2 . _2) & expects True

    it "use: getter for state" $ do
      (1 :: Int, 2 :: Int) & evalState (use _1) & expects 1
      (1 :: Int, 2 :: Int) & evalState (use _2) & expects 2
      (1 :: Int, 2 :: Int) & evalState (uses _1 show) & expects "1"
      (1 :: Int, 2 :: Int) & evalState (uses _2 show) & expects "2"

    it "listening: getter for writer" $ do
      tell "Hello" & listening (to (toUpper <$>)) & runWriter & expects (((), "HELLO"), "Hello")
      tell "Hello" & listenings (to (toUpper <$>)) (<> "WORLD") & runWriter & expects (((), "HELLOWORLD"), "Hello")
      tell "Hello" & listening (to head) & runWriter & expects (((), 'H'), "Hello")

    it "to: build a getter" $ do
      1 & view (to negate) & expects @Int (-1)
      1 & view (to succ) & expects @Int 2
      (1 :: Int) & view (to show) & expects "1"
      [1 .. 10] & view (to head) & expects @Int 1
      [1 .. 10] & view (to init) & expects @[Int] [1 .. 9]

    it "like: constant getter" $ do
      True & view (like 10) & expects @Int 10
      "A" & view (like 10) & expects @Int 10

  it "Setter" $ do
    (1 :: Int, 2 :: Int) & set _1 "Hello" & expects ("Hello", 2)
    (1 :: Int, 2 :: Int) & set _2 "Hello" & expects (1, "Hello")
    (1 :: Int, 2 :: Int) & over _1 (* 10) & expects (10, 2)
    (1 :: Int, 2 :: Int) & over _2 (* 10) & expects (1, 20)

    [1, 2, 3] & over mapped (* 2) & expects [2, 4, 6 :: Int]

  describe "Fold" $ do
    it "Basic" $ do
      -- preview
      [1 .. 5 :: Int] & preview folded & expects (Just @Int 1)
      [1 .. 5 :: Int] & preview (folded . timesN 2) & expects (Just @Int 2)
      1 & preview repeated & expects (Just @Int 1)
      1 & preview (repeated . timesN 2) & expects (Just @Int 2)

      -- toListOf
      [1 .. 5 :: Int] & toListOf folded & expects [1 .. 5]

      -- composition
      [1 .. 5 :: Int] & toListOf (backwards folded) & expects [5, 4, 3, 2, 1]
      [1 .. 5 :: Int] & toListOf (folded . addN 1) & expects [2, 3, 4, 5, 6]
      [1 .. 5 :: Int] & toListOf (folded . addN 1 . filtered even) & expects [2, 4, 6]

    it "unfolding - generate Fold from single value" $ do
      -- generator
      1 & toListOf (replicated 5) & expects [1, 1, 1, 1, 1 :: Int]
      1 & toListOf (replicated 5 . timesN 2) & expects [2, 2, 2, 2, 2 :: Int]
      1 & toListOf repeated & take 3 & expects [1, 1, 1 :: Int]
      1 & toListOf (taking 3 repeated) & expects [1, 1, 1 :: Int]
      [1, 2, 3 :: Int] & toListOf (taking 5 $ cycled folded) & expects [1, 2, 3, 1, 2]
      1 & toListOf (taking 5 $ iterated (* 10)) & expects [1, 10, 100, 1000, 10000 :: Int]

      -- unfolded
      1 & toListOf (unfolded (\x -> if x > 100 then Nothing else Just (x, x * 10))) & expects [1, 10, 100 :: Int]
      1 & toListOf (unfolded (\x -> if x > 100 then Nothing else Just (x, x * 10)) . addN 1) & expects [2, 11, 101 :: Int]

    it "[folding] build Fold from normal function" $ do
      [1 .. 5 :: Int] & toListOf (folding id) & expects [1 .. 5]
      [1 .. 5 :: Int] & toListOf (folding id) & expects (toListOf folded [1..5])
      [1 .. 5 :: Int] & toListOf (folding ((* 2) <$>)) & expects [2, 4, 6, 8, 10]
      [1 .. 5 :: Int] & toListOf (folding ((* 2) <$>)) & expects (toListOf (folded . timesN 2) [1..5])

    it "Fold -> Fold" $ do
      [1 .. 10] & toListOf (takingWhile (< 5) folded) & expects [1, 2, 3, 4 :: Int]
      [1 .. 10] & toListOf (droppingWhile (< 5) folded) & expects [5, 6, 7, 8, 9, 10 :: Int]

    it "builtins" $ do
      [1 .. 5 :: Int] & foldMapOf folded Sum & expects 15
      [1 .. 5 :: Int] & foldOf (folded . to Sum) & expects 15
      [1 .. 5 :: Int] & foldOf (folded . filtered (< 3) . to Sum) & expects 3

      [1 .. 5 :: Int] & allOf folded (== 1) & expects False
      [1 .. 5 :: Int] & anyOf folded (== 1) & expects True
