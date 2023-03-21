{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TupleSections #-}

module Chapter14.Lens.Spec
  ( spec,
    Alpha (..),
  )
where

import Chapter14.Lens.Tree
import Control.Arrow (second)
import Control.Lens
import Control.Monad.Trans.State
import Control.Monad.Trans.Writer
import Data.Char
import Data.Monoid
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

tree1 :: Tree Int
tree1 = Node (Node (Leaf 1) (Leaf 10)) (Leaf 3)

spec :: Spec
spec = describe "Lens" $ do
  describe "Tree" $ do
    it "gets" $ do
      tree1 & preview tleft & expects $ Just $ Node (Leaf 1) (Leaf 10)
      tree1 & preview (tleft . tleft) & expects $ Just (Leaf 1)
      tree1 & preview (tleft . tleft . tvalue) & expects (Just 1)
      tree1 & preview (tleft . tright . tvalue) & expects (Just 10)
      tree1 & preview tright & expects $ Just (Leaf 3)
      tree1 & preview (tright . tvalue) & expects $ Just 3
      tree1 & preview (tleft . tleft . tleft) & expects Nothing
      tree1 & preview (tleft . tright . tright) & expects Nothing
      tree1 & preview (tright . tleft) & expects Nothing

    it "manual lens" $ do
      tree1 & preview (tLEFT . tLEFT) & expects $ Just (Leaf 1)
      tree1 & preview (tLEFT . tLEFT . tVALUE) & expects (Just 1)
      tree1 & preview (tLEFT . tRIGHT . tVALUE) & expects (Just 10)
      tree1 & preview (tLEFT . tLEFT . tLEFT) & expects Nothing
      tree1 & preview (tLEFT . tRIGHT . tRIGHT) & expects Nothing
      tree1 & preview (tRIGHT . tLEFT) & expects Nothing

    it "sets" $ do
      tree1 & set tleft (Leaf 100) & expects $ Node (Leaf 100) (Leaf 3)
      tree1 & set (tleft . tleft) (Leaf 100) & expects $ Node (Node (Leaf 100) (Leaf 10)) (Leaf 3)

      -- no change
      tree1 & set (tleft . tright . tright) (Leaf 100) & expects $ tree1

    it "folds" $ do
      tree1 & foldOf (folded . to (: [])) & expects [1, 10, 3]
      tree1 & foldOf (folded . to Sum) & expects $ Sum 14
      tree1 & foldMapOf folded (: []) & expects [1, 10, 3]
      tree1 & foldMapOf folded Sum & expects $ Sum 14

  describe "Getter" $ do
    it "[view]" $ do
      -- trivial
      (A, B) & view id & expects (A, B)

      (A, (C, (D, True))) & view _1 & expects A
      (A, (C, (D, True))) & view _2 & expects (C, (D, True))
      (A, (C, (D, True))) & view (_2 . _1) & expects C
      (A, (C, (D, True))) & view (_2 . _2) & expects (D, True)
      (A, (C, (D, True))) & view (_2 . _2 . _1) & expects D
      (A, (C, (D, True))) & view (_2 . _2 . _2) & expects True
      (A, (C, (D, Just True))) & preview (_2 . _2 . _2 . _Just) & expects $ Just True

    it "[use] getter for state" $ do
      (A, B) & evalState (use _1) & expects A
      (A, B) & evalState (use _2) & expects B

    it "[listening] getter for writer" $ do
      tell A & listening id & runWriter & expects (((), A), A)
      tell A >> tell B & listening id & runWriter & expects (((), B), B)
      tell A & listening (to show) & runWriter & expects (((), "A"), A)

    it "[to] build a getter" $ do
      1 & view (to negate) & expects @Int (-1)
      (1, 'a') & view (_1 . to negate) & expects @Int (-1)

    it "[ito] build an indexed getter" $ do
      [A .. E] & iview (folded . ito (Sum (1 :: Int),)) & expects (5, E)
      A & iview (ito (0 :: Int,)) & expects (0, A)
      zip [1 :: Int ..] [A .. E] & elemIndexOf (folded . ito id) A & expects $ Just 1
      zip [1 :: Int ..] [A .. E] & elemIndexOf (folded . ito id) B & expects $ Just 2
      zip [1 :: Int ..] [A .. E] & elemIndexOf (folded . ito id) Z & expects $ Nothing

    it "[like] constant getter" $ do
      () & view (like A) & expects A
      () & view (like A . to succ) & expects B

  describe "Setter" $
    it "Setter" $ do
      (A, B) & set _1 "Hello" & expects ("Hello", B)
      (A, B) & set _2 "Hello" & expects (A, "Hello")
      (A, B) & over _1 succ & expects (B, B)
      (A, B) & over _2 succ & expects (A, C)

  describe "Fold" $ do
    it "Basic" $ do
      [A, B] & foldOf folded & expects B
      [A, B] & foldOf (folded . to show) & expects "AB"
      [A, B] & foldMapOf folded show & expects "AB"
      [A, B] & foldByOf folded (<>) mempty & expects B

    it "Fold target" $ do
      -- Fold over object
      [A, B] & foldOf folded & expects B
      Just A & foldOf folded & expects A

      -- Fold over child object
      [ ([A, B, C], ()),
        ([X, Y], ())
        ]
        & foldOf (element 0 . _1 . folded . to show)
        & expects "ABC"
      [ ([A, B, C], ()),
        ([X, Y], ())
        ]
        & foldOf (element 1 . _1 . folded . to show)
        & expects "XY"

      -- Nested folds
      [ ([A, B, C], ()),
        ([X, Y], ())
        ]
        & foldOf (folded . _1 . folded . to show)
        & expects "ABCXY"

    it "[pre,preview] fold to first item" $ do
      [(A, B), (C, D)] & view (pre folded) & expects $ Just (A, B)
      [(A, B), (C, D)] & view (pre (element 0)) & expects $ Just (A, B)
      [1, 2] & view (pre folded) & expects $ Just (1 :: Int)
      [1, 2] & preview folded & expects $ Just (1 :: Int)

      [(A, False), (C, True)] & preview folded & expects $ Just (A, False)
      [(A, False), (C, True)] & preview (folded . _1) & expects $ Just A
      [(A, False), (C, True)] & preview (folded . _2) & expects $ Just False
      ([] :: [(Bool, Bool)]) & preview (folded . _2) & expects $ Nothing

      (A, B) & preview repeated & expects (Just (A, B))
      (A, B) & preview (repeated . _1) & expects (Just A)
      (A, B) & preview (repeated . _2) & expects (Just B)

    it "[toListOf] fold to list" $ do
      [A .. C] & toListOf folded & expects [A, B, C]
      zip [A .. C] [1 :: Int ..] & toListOf (folded . _1) & expects [A, B, C]
      zip [A .. C] [1 ..] & toListOf (folded . _2) & expects [1, 2, 3 :: Int]

    it "[folding] build Fold from value" $ do
      [A .. D] & foldOf (folding id) & expects (A <> B <> C <> D)
      [A .. D] & foldOf (folding (show <$>)) & expects "ABCD"
      [A .. D] & foldOf (folded . to show) & expects "ABCD"

      -- folding on inner
      [ [A, B, C],
        [D, E]
        ]
        & foldOf (element 0 . folding id)
        & expects
        $ A <> B <> C
      [ [A, B, C],
        [D, E]
        ]
        & foldOf (element 1 . folding id)
        & expects
        $ D <> E
      [ [A, B, C],
        [D, E]
        ]
        & foldOf (element 10 . folding id)
        & expects mempty

    it "[combinators]" $ do
      A & toListOf (unfolded (\x -> if x > D then Nothing else Just (x, succ x))) & expects [A, B, C, D]
      (A, B) & toListOf (iterated (second succ) . _2) & take 3 & expects [B, C, D]
      [A .. D] & toListOf (folded . filtered (> B)) & expects [C, D]
      (A, B, 'a') & view (_1 . filtered (> A)) & expects NA
      (A, B, 'a') & view (_2 . filtered (> A)) & expects B
      [A .. D] & toListOf (backwards folded) & expects [D, C, B, A]

      (A, B) & toListOf (repeated . _2) & take 3 & expects [B, B, B]
      (A, B) & toListOf (replicated 3 . _1) & expects [A, A, A]
      (A, [B, C]) & toListOf (cycled (_2 . folded)) & take 3 & expects [B, C, B]

      -- get infinite 
      [1 :: Int ..] & toListOf (takingWhile (< 4) traverse) & expects [1,2,3]
      -- set infinite 
      [1 :: Int ..] & set (takingWhile (< 4) traverse) 10 & take 5 & expects [10,10,10,4,5]

      [A .. D] & toListOf (takingWhile (< C) folded) & expects [A, B]
      [A .. D] & toListOf (takingWhile (< C) traverse) & expects [A, B]
      [A .. D] & toListOf (takingWhile (< C) traverse) & expects [A, B]
      (A, B, C) & preview (takingWhile (> A) _1) & expects Nothing
      (A, B, C) & preview (takingWhile (> A) _2) & expects (Just B)
      (A, B, Just C) & preview (takingWhile (> A) (_3 . _Just)) & expects (Just C)
      (A, B, Nothing) & preview (takingWhile (> A) (_3 . _Just)) & expects Nothing
      [A .. E] & toListOf (droppingWhile (< C) folded) & expects [C .. E]
      [A .. E] & toListOf (droppingWhile (< C) traverse) & expects [C .. E]

      [A .. D] & set (takingWhile (< C) traverse) Z & expects [Z, Z, C, D]

    it "[Prelude]" $ do
      let x =
            zip3
              [A, B, C, D, E]
              (print <$> [A ..])
              $ zip [1 :: Int ..]
              $ zip ['a' ..]
              $ zip (cycle [True, False])
              $ cycle ["a", "b", "c"]

      x & toListOf (folded . _3) & print

      x & foldMapOf (folded . _3 . _1) Sum & expects 15

      x & allOf (folded . _1) (< Z) & expects True
      x & anyOf (folded . _1) (> A) & expects True
      x & noneOf (folded . _1) (> A) & expects False
      x & andOf (folded . _3 . _2 . _2 . _1) & expects False
      x & orOf (folded . _3 . _2 . _2 . _1) & expects True

      x & productOf (folded . _3 . _1) & expects 120
      x & sumOf (folded . _3 . _1) & expects 15

      x & traverseOf_ (folded . _1) print & expectsIOCapture "A\nB\nC\nD\nE\n"
      forOf_ (folded . _1) x print & expectsIOCapture "A\nB\nC\nD\nE\n"
      x & sequenceAOf_ (folded . _2) & expectsIOCapture "A\nB\nC\nD\nE\n"
      x & mapMOf_ (folded . _1) print & expectsIOCapture "A\nB\nC\nD\nE\n"
      forMOf_ (folded . _1) x print & expectsIOCapture "A\nB\nC\nD\nE\n"
      x & sequenceOf_ (folded . _2) & expectsIOCapture "A\nB\nC\nD\nE\n"

      x & asumOf (folded . _2) & expectsIOCapture "A\n"
      x & msumOf (folded . _2) & expectsIOCapture "A\n"

      x & concatMapOf (folded . _3 . _1) show & expects "12345"
      x & concatOf (folded . _3 . _2 . _2 . _2) & expects "abcab"

      x & elemOf (folded . _1) A & expects True
      x & notElemOf (folded . _1) A & expects False
      x & lengthOf (folded . _1) & expects 5
      x & nullOf (folded . _1) & expects False
      x & firstOf (folded . _1) & expects $ Just A
      x & lastOf (folded . _1) & expects $ Just E
      x & maximumOf (folded . _1) & expects $ Just E
      x & minimumOf (folded . _1) & expects $ Just A
      x & findOf (folded . _1) (== A) & expects $ Just A
      x & findMOf (folded . _1) (pure . (== A)) & expectsIO (Just A)
      x & lookupOf (folded . _3) 1 & expects (Just ('a', (True, "a")))

    it "indexed" $ do
      [1 .. 5 :: Int] & ifoldMapOf folded (\i v -> Sum $ i + v) & expects 25
      [1 .. 5 :: Int] & ifoldMapOf folded (\i _ -> Sum i) & expects 10
      [1 .. 5 :: Int] & ifoldMapOf folded (\_ v -> Sum v) & expects 15
      [1 .. 5 :: Int] & ifoldMapOf (folded . to (* 10)) (\_ v -> Sum v) & expects 150
      [1 .. 5 :: Int] & ifoldMapOf (folded . to (* 10)) (\i _ -> Sum i) & expects 10

      [1 .. 5 :: Int] & itoListOf (folded . to (* 2)) & expects [(0, 2), (1, 4), (2, 6), (3, 8), (4, 10)]
      [1 .. 5 :: Int] & ianyOf folded (\i _ -> i < 10) & expects True

  describe "Traversal" $ do
    it "[traverseOf]" $ do
      -- trivial
      [A, B, C] & traverseOf traverse pure & expectsIO [A, B, C]

      -- traverse
      [(Just 1 :: Maybe Int, 2 :: Int), (Just 3, 4)] & traverseOf traversed pure & expectsIO [(Just 1, 2), (Just 3, 4)]
      [(Just 1 :: Maybe Int, 2 :: Int), (Just 3, 4)] & traverseOf (traversed . _1 . _Just) (pure . (* 10)) & expectsIO [(Just 10, 2), (Just 30, 4)]
      [(Just 1 :: Maybe Int, 2 :: Int), (Nothing, 4)] & traverseOf (traversed . _1 . _Just) (pure . (* 10)) & expectsIO [(Just 10, 2), (Nothing, 4)]

      -- settter
      [(Just 1 :: Maybe Int, 2 :: Int), (Nothing, 4)] & set (traversed . _1 . _Just) True & expects [(Just True, 2), (Nothing, 4)]

    it "Traverse target" $ do
      -- trivial
      [A, B, C] & traverseOf traverse (pure . succ) & expectsIO [B, C, D]

      -- traverse over child of object
      ([A, B], (D, E), Just (Just [F]))
        & traverseOf (_1 . traverse) (pure . succ)
        & expectsIO ([B, C], (D, E), Just (Just [F]))
      ([A, B], (D, E), Just (Just [F]))
        & traverseOf (_2 . traverse) (pure . succ)
        & expectsIO ([A, B], (D, F), Just (Just [F]))
      ([A, B], (D, E), Just (Just [F]))
        & traverseOf (_3 . _Just . _Just . traverse) (pure . succ)
        & expectsIO ([A, B], (D, E), Just (Just [G]))
      ([A, B], (D, E), Just (Just [F]))
        & traverseOf (_3 . _Just . traverse . traverse) (pure . succ)
        & expectsIO ([A, B], (D, E), Just (Just [G]))
      ([A, B], (D, E), Just (Just [F]))
        & traverseOf (_3 . traverse . traverse . traverse) (pure . succ)
        & expectsIO ([A, B], (D, E), Just (Just [G]))
      ([A, B], (D, E), Just (Just [F]))
        & traverseOf (_3 . traverse . _Just . element 0) (pure . succ)
        & expectsIO ([A, B], (D, E), Just (Just [G]))
      ([A, B], (D, E), Just (Just [F]))
        & traverseOf (_3 . traverse . _Just . element 1) (pure . succ)
        & expectsIO ([A, B], (D, E), Just (Just [F]))

      -- traverse over child of traversable
      [(A, B), (C, D)]
        & traverseOf (traverse . _1) (pure . succ)
        & expectsIO [(B, B), (D, D)]
      [(A, B), (C, D)]
        & traverseOf (traverse . _2) (pure . succ)
        & expectsIO [(A, C), (C, E)]

      -- traverse over nested
      [ ([A, B], Just C),
        ([D, E], Nothing)
        ]
        & traverseOf (traverse . _1 . traverse) (pure . succ)
        & expectsIO [([B, C], Just C), ([E, F], Nothing)]
      [ ([A, B], Just C),
        ([D, E], Nothing)
        ]
        & traverseOf (traverse . _2 . traverse) (pure . succ)
        & expectsIO [([A, B], Just D), ([D, E], Nothing)]
      [ ([A, B], Just [X, Y]),
        ([D, E], Nothing),
        ([], Just [Z])
        ]
        & traverseOf (element 0 . _2 . traverse . traverse) (pure . succ)
        & expectsIO [([A, B], Just [Y, Z]), ([D, E], Nothing), ([], Just [Z])]
      [ ([A, B], Just [X, Y]),
        ([D, E], Nothing),
        ([], Just [Z])
        ]
        & traverseOf (element 1 . _2 . traverse . traverse) (pure . succ)
        & expectsIO [([A, B], Just [X, Y]), ([D, E], Nothing), ([], Just [Z])]
      [ ([A, B], Just [X, Y]),
        ([D, E], Nothing),
        ([], Just [Z])
        ]
        & traverseOf (element 2 . _2 . traverse . traverse) (pure . pred)
        & expectsIO [([A, B], Just [X, Y]), ([D, E], Nothing), ([], Just [Y])]

    it "[elements] value by index" $ do
      -- inner
      [((), [A .. E]), ((), [D])] & preview (elementOf (traverse . _2 . traverse) 1) & expects $ Just B
      [((), [A .. E]), ((), [D])] & preview (elementOf (traverse . _2 . traverse) 1) & expects $ Just B

      -- get single element
      [A .. E] & preview (element 0) & expects $ Just A
      [A .. E] & preview (element 1) & expects $ Just B
      [A .. E] & preview (element 1000) & expects Nothing

      -- get multiple elements
      [A .. E] & toListOf (elements (< 3)) & expects [A, B, C]
      [A .. E] & foldMapOf (elements (< 3)) show & expects "ABC"

      "abcd" & set (element 0) 'A' & expects "Abcd"
      "abcd" & set (element 1000) 'A' & expects "abcd"
      "abcd" & set (elementOf traversed 1) 'A' & expects "aAcd"
      "abcd" & set (elementOf traverse 1) 'A' & expects "aAcd"

      "abcd" & preview (element 1) & expects (Just 'b')
      "abcd" & preview (element 100) & expects Nothing
      "abcd" & preview (elementOf traversed 1) & expects (Just 'b')
      "abcd" & preview (elementOf (traversed . to ord) 1) & expects (Just 98)

      "abcd" & toListOf (elements (> 1)) & expects "cd"
      "abcd" & preview (elements (> 1)) & expects (Just 'c')
      "abcd" & set (elements (> 1)) '9' & expects "ab99"

    it "[elements] nested" $ do
      -- nested
      [ [[A, B], [C, D]],
        [[X, Y], [Z, W]]
        ]
        & preview (element 0 . element 0 . element 0)
        & expects
        $ Just A
      [ [[A, B], [C, D]],
        [[X, Y], [Z, W]]
        ]
        & preview (element 0 . element 0 . element 1)
        & expects
        $ Just B
      [ [[A, B], [C, D]],
        [[X, Y], [Z, W]]
        ]
        & preview (element 0 . element 1 . element 1)
        & expects
        $ Just D
      [ [[A, B], [C, D]],
        [[X, Y], [Z, W]]
        ]
        & preview (element 1 . element 0 . element 1)
        & expects
        $ Just Y
      [ [[A, B], [C, D]],
        [[X, Y], [Z, W]]
        ]
        & preview (element 100 . element 0 . element 1)
        & expects
        $ Nothing
      [ [[A, B], [C, D]],
        [[X, Y], [Z, W]]
        ]
        & toListOf (elements (< 10) . elements (< 10) . elements (< 10))
        & expects [A, B, C, D, X, Y, Z, W]
      [ [[A, B], [C, D]],
        [[X, Y], [Z, W]]
        ]
        & toListOf (elements (< 10) . elements (< 10) . element 0)
        & expects [A, C, X, Z]
      [ [[A, B], [C, D]],
        [[X, Y], [Z, W]]
        ]
        & toListOf (elements (< 10) . elements (< 10) . element 1)
        & expects [B, D, Y, W]
      [ [[A, B], [C, D]],
        [[X, Y], [Z, W]]
        ]
        & toListOf (elements (< 10) . element 0 . element 0)
        & expects [A, X]
      [ [[A, B], [C, D]],
        [[X, Y], [Z, W]]
        ]
        & toListOf (elements (< 10) . element 0 . elements (< 10))
        & expects [A, B, X, Y]

    describe "At" $ do
      it "[ix]" $ do
        "abcde" & set (ix 2) '?' & expects "ab?de"
        "abcde" & over (ix 2) toUpper & expects "abCde"
        "abcde" & preview (ix 2) & expects (Just 'c')
        [] & preview (ix 2) & expects @(Maybe String) Nothing

  it "scratch space" $ do
    ( [ Just ["A", "B"],
        Nothing
      ],
      ()
      )
      & foldOf (_1 . element 1 . _Just . folded)
      & expects ""
    [ ("A", [Just "B", Just "C"]),
      ("X", [Just "Y", Nothing])
      ]
      & foldOf (folded . _2 . element 1 . _Just)
      & expects "C"
    [ ([A, X], [[C, Z], [E, A]]),
      ([S, T], [[U], [V, X]])
      ]
      & foldOf (folded . _2 . folded . element 10)
      & expects NA

-- ig2 = ig _2
