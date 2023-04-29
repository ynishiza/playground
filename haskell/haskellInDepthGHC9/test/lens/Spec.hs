{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# HLINT ignore "Take on a non-positive" #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Spec
  ( spec,
  )
where

import Control.Lens qualified as L
import Control.Monad.State
import Data.Char
import Data.Function
import Data.Tuple
import Lens
import Lens.Scratch (deferAp)
import Lens.TH
import Person
import System.IO.Error qualified as E
import Test.Hspec
import Tree

data Alpha = NotAlpha | A | B | C | D | E | F | G | H | I | J | K | L | M | N | O | P | Q | R | S | T | U | V | W | X | Y | Z
  deriving (Eq, Ord, Show, Bounded, Enum)

instance Semigroup Alpha where
  a <> b = toEnum $ max (fromEnum a) (fromEnum b)

instance Monoid Alpha where
  mempty = NotAlpha

data Point where
  Point :: {coordX :: Double, coordY :: Double} -> Point
  deriving stock (Show, Eq)

data Segment where
  Segment :: {startPoint :: Point, endPoint :: Point} -> Segment
  deriving stock (Show, Eq)

$(genFieldLenses ''Point)
$(genFieldLenses ''Segment)

expects :: (Show a, Eq a) => a -> a -> Expectation
expects expected value = value `shouldBe` expected

expectsIO :: (Show a, Eq a) => a -> IO a -> Expectation
expectsIO expected io = io >>= (`shouldBe` expected)

shouldNotHappen :: a
shouldNotHappen = error "Should not happen"

spec :: Spec
spec = describe "" $ do
  it "Lens" $ do
    (A, True)
      & view _1
      & expects (L.view L._1 (A, True))
    (A, True)
      & view _2
      & expects True

  it "Folds are monoids (https://www.haskellforall.com/2021/09/optics-are-monoids.html)" $ do
    (A, B)
      & view (_1 <> _2)
      & expects (A <> B)
    zip [1 :: Int ..] [A .. E]
      & toListOf (traverse . (_1 <> (_2 . to ((* 10) . fromEnum))))
      & expects [1, 10, 2, 20, 3, 30, 4, 40, 5, 50]

  it "[Each]" $ do
    -- fold
    (A, B, C)
      & toListOf (each @Int)
      & expects [A, B, C]
    [A .. D]
      & toListOf (each @Int)
      & expects [A, B, C, D]
    (A, B, C)
      & itoListOf each
      & expects [(0 :: Int, A), (1, B), (2, C)]
    [A .. D]
      & itoListOf each
      & expects [(0 :: Int, A), (1, B), (2, C), (3, D)]

    -- set
    (A, B, C)
      & set (each @Int) False
      & expects (False, False, False)
    (A, B, C)
      & over (each @Int) succ
      & expects (B, C, D)
    [A .. D]
      & over (each @Int) succ
      & expects [B, C, D, E]

  describe "User defined data types" $ do
    describe "[Record data type] person" $ do
      let personA = Person "A" A
          personB = Person "B" B
          persons = [personA, personB]

      it "gets" $ do
        personA
          & view _name
          & expects "A"
        personA
          & view _value
          & expects A
        persons
          & toListOf (traverse . _name)
          & expects ["A", "B"]

      it "sets" $ do
        personA
          & set _name "a"
          & expects (Person "a" A)
        personA
          & set _value Z
          & expects (Person "A" Z)

    describe "[Recursive data type] Tree" $ do
      it "gets" $ do
        let t :: Tree Int
            t =
              Node
                ( Node
                    (Leaf 1)
                    ( Node
                        (Leaf 10)
                        (Leaf 3)
                    )
                )
                (Leaf (-1))
        -- get
        t
          & preview _RightTree
          & expects
          $ Just (Leaf (-1))
        t
          & preview (_RightTree . _Leaf)
          & expects
          $ Just (-1)
        t
          & preview (_RightTree . _LeftTree)
          & expects Nothing
        t
          & preview (_RightTree . _RightTree)
          & expects Nothing

        t
          & preview (_LeftTree . _LeftTree . _Leaf)
          & expects
          $ Just 1
        t
          & preview (_LeftTree . _RightTree . _LeftTree . _Leaf)
          & expects
          $ Just 10
        t
          & preview (_LeftTree . _RightTree . _RightTree . _Leaf)
          & expects
          $ Just 3
        t
          & preview (_LeftTree . _LeftTree)
          & expects
          $ Just (Leaf 1)
        t
          & preview _Leaf
          & expects Nothing

      it "sets" $ do
        let t :: Tree Int
            t =
              Node
                ( Node
                    (Leaf 1)
                    (Leaf 10)
                )
                (Leaf 4)

        t
          & set traverse 'a'
          & expects
          $ Node
            ( Node
                (Leaf 'a')
                (Leaf 'a')
            )
            (Leaf 'a')
        t
          & set mapped 'a'
          & expects
          $ Node
            ( Node
                (Leaf 'a')
                (Leaf 'a')
            )
            (Leaf 'a')
        t
          & over traverse (* 10)
          & expects
          $ Node
            ( Node
                (Leaf 10)
                (Leaf 100)
            )
            (Leaf 40)

      it "profunctor and lens equality" $ do
        let t :: Tree Int
            t =
              Node
                ( Node
                    (Leaf 1)
                    ( Node
                        (Leaf 10)
                        (Leaf 3)
                    )
                )
                (Leaf (-1))

        expects
          (preview _LeftTree t)
          (preview _LeftTree' t)
        expects
          (preview _RightTree t)
          (preview _RightTree' t)

  describe "Lens.Index" $ do
    it "[reindex]" $ do
      [A .. D]
        & itoListOf (reindex (* 100) folded)
        & expects [(0, A), (100, B), (200, C), (300, D)]

    it "[indexing]" $ do
      [A .. D]
        & itoListOf (indexing traverse)
        & expects [(0, A), (1, B), (2, C), (3, D)]

    it "[index, indices]" $ do
      [A .. D]
        & toListOf (folded . indices (even @Int))
        & expects [A, C]
      [A .. D]
        & toListOf (folded . index 0)
        & expects [A]
      [A .. D]
        & toListOf (folded . index 1)
        & expects [B]
      [A .. D]
        & toListOf (folded . index 100)
        & expects []

    it "[withIndex, asIndex]" $ do
      [A .. D]
        & toListOf (folded . asIndex)
        & expects [0, 1, 2, 3]
      [A .. D]
        & toListOf (folded . asIndex . to (even @Int))
        & expects [True, False, True, False]
      [A .. D]
        & toListOf (folded . withIndex)
        & expects [(0, A), (1, B), (2, C), (3, D)]

    it "[<.>]" $ do
      [[A, B], [C, D, E]]
        & itoListOf (folded <.> folded)
        & expects [((0, 0), A), ((0, 1), B), ((1, 0), C), ((1, 1), D), ((1, 2), E)]

      [[A, B], [C, D, E]]
        & itoListOf (folded <. folded)
        & expects [(0, A), (0, B), (1, C), (1, D), (1, E)]

      [[A, B], [C, D, E]]
        & itoListOf (folded .> folded)
        & expects [(0, A), (1, B), (0, C), (1, D), (2, E)]

      [A .. D]
        & itoListOf (folded <.> ito (\x -> (fromEnum x * 10, x)))
        & expects [((0, 10), A), ((1, 20), B), ((2, 30), C), ((3, 40), D)]

      [A .. D]
        & itoListOf (folded <.> (ito (\x -> (fromEnum x, x)) . withIndex))
        & expects [((0, 1), (1, A)), ((1, 2), (2, B)), ((2, 3), (3, C)), ((3, 4), (4, D))]

  describe "Lens.Get" $ do
    let deepData =
          ( A,
            ( True,
              ( 1 :: Int,
                ( 'c',
                  ( "abcd",
                    ( [1 :: Int .. 100],
                      [1 :: Int ..]
                    )
                  )
                )
              )
            )
          )

    it "[Monoid] get multiple values" $ do
      (A, (B, C))
        & toListOf (_1 <> _2 . _1)
        & expects [A, B]
      (A, (B, C))
        & toListOf (_1 <> _1)
        & expects [A, A]

      (A, (B, C))
        & view (_1 <> _2 . _1)
        & expects B
      (A, (B, C))
        & view (_1 <> _1)
        & expects A

    it "[view] get" $ do
      deepData
        & view _1
        & expects A
      deepData
        & view (_2 . _1)
        & expects True
      deepData
        & view (_2 . _2 . _1)
        & expects 1
      deepData
        & toListOf (_2 . _2 . _2 . _2 . _1 . traverse . to toUpper)
        & expects "ABCD"

      deepData
        & views _1 show
        & expects "A"
      deepData
        & views (_2 . _1) show
        & expects "True"

    it "[use] get state" $ do
      let s = liftIO $ print "Hello"
      (s >> use _1)
        & flip evalStateT deepData
        & expectsIO A
      (s >> use (_2 . _1))
        & flip evalStateT deepData
        & expectsIO True
      (s >> uses _1 succ)
        & flip evalStateT deepData
        & expectsIO B

    describe "Lenses" $ do
      it "[like] constant values" $ do
        -- like: constant
        deepData
          & view (_1 . like 'a')
          & expects 'a'
        deepData
          & view (_2 . like 'a')
          & expects 'a'
        deepData
          & toListOf (_2 . _2 . _2 . _2 . _1 . traverse . like 'a')
          & expects
          $ "aaaa"

      it "[to, ito] map getter" $ do
        'a'
          & view (to succ)
          & expects 'b'
        deepData
          & view (_1 . to succ)
          & expects B
        deepData
          & view (_2 . _2 . _1 . to succ)
          & expects 2
        deepData
          & toListOf (_2 . _2 . _2 . _2 . _1 . traverse . to toUpper)
          & expects "ABCD"

        -- chain
        ["1", "2", "3", "4", "5", "10"]
          & toListOf (traverse . to (read @Int) . to (\i -> if even i then Just i else Nothing) . _Just)
          & expects [2, 4, 10]

        -- ito
        zip [A .. E] [1 ..]
          & itoListOf (traverse . ito swap)
          & expects [(1 :: Int, A), (2, B), (3, C), (4, D), (5, E)]

      it "[ito'] map preserving indices" $ do
        [A .. E]
          & itoListOf (indexing traverse . ito' (\i x -> (i * 10, x)))
          & expects [(0, A), (10, B), (20, C), (30, D), (40, E)]

        -- ito' = withIndex . ito
        [A .. E]
          & itoListOf (indexing traverse . withIndex . ito (\(i, x) -> (i * 10, x)))
          & expects [(0, A), (10, B), (20, C), (30, D), (40, E)]

  describe "Lens.Fold" $ do
    it "folds" $ do
      zip [A .. E] ['a' ..]
        & foldOf (traverse . _1)
        & expects E
      zip [A .. E] ['a' ..]
        & foldrOf (traverse . _2) (:) ""
        & expects "abcde"
      zip [A .. E] ['a' ..]
        & preview (traverse . _1)
        & expects
        $ Just A
      zip [A .. E] ['a' ..]
        & preview (traverse . _2)
        & expects
        $ Just 'a'
      []
        & preview traverse
        & expects (Nothing :: Maybe ())

    describe "Lenses" $ do
      it "[folded] traverse with index" $ do
        -- folded
        zip [A .. E] ['a' ..]
          & toListOf (folded . _1)
          & expects [A .. E]
        zip [A .. E] ['a' ..]
          & toListOf (folded . _2)
          & expects "abcde"

        -- indexed
        [A .. E]
          & elemIndexOf folded A
          & expects
          $ (Just 0 :: Maybe Int)
        [A .. E]
          & elemIndexOf folded C
          & expects
          $ (Just 2 :: Maybe Int)
        [A .. E]
          & elemIndexOf folded Z
          & expects (Nothing :: Maybe Int)

        -- case: infinite
        (A, [1 :: Int ..])
          & preview (_2 . folded)
          & expects
          $ Just 1
        (A, [1 :: Int ..])
          & toListOf (_2 . folded)
          & take 3
          & expects
          $ [1, 2, 3]
        zip [1 :: Int ..] (repeat A)
          & toListOf (folded . _1)
          & take 3
          & expects
          $ [1, 2, 3]
        zip [1 :: Int ..] (repeat A)
          & toListOf (folded . _2)
          & take 3
          & expects
          $ [A, A, A]

      it "[foldring]" $ do
        [A .. D]
          & toListOf (foldring foldr)
          & expects [A, B, C, D]
        [A .. E]
          & firstOf (foldring foldr)
          & expects (Just A)
        [A .. E]
          & lastOf (foldring foldr)
          & expects (Just E)

      it "[replicate, repeat]" $ do
        -- replicate, repeat
        (A, B)
          & toListOf (_1 . replicated 3)
          & expects [A, A, A]

        (A, B)
          & toListOf (_1 . repeated)
          & take 0
          & expects []

        (A, B)
          & toListOf (_1 . repeated)
          & take 5
          & expects [A, A, A, A, A]

        (A, B)
          & toListOf (_1 . iterated (succ . succ))
          & take 5
          & expects [A, C, E, G, I]

      it "[filtered]" $ do
        [A .. E]
          & toListOf (traverse . filtered (> C))
          & expects [D, E]
        [A .. E]
          & toListOf (traverse . filtered (> C) . to show)
          & expects ["D", "E"]

        -- non-list
        (A, B)
          & view (_1 . filtered (> A))
          & expects mempty
        (A, B)
          & view (_2 . filtered (> A))
          & expects B

      it "[backwards]" $ do
        [A .. E]
          & toListOf (backwards traverse)
          & expects [E, D, C, B, A]

        -- any applicative
        (A, B)
          & view (backwards _1)
          & expects A
        (A, B)
          & view (backwards id)
          & expects (A, B)

      it "[has] check if empty" $ do
        -- has
        A
          & has id
          & expects True
        ([A .. E], [])
          & has (_1 . traverse)
          & expects True
        ([A .. E], [])
          & has (_2 . traverse)
          & expects False

      describe "[taking, dropping]" $ do
        it "take,drop" $ do
          let iterateSucc = iterated succ
          -- taking/dropping
          (A, B)
            & preview (takingWhile (_1 . iterateSucc) (< D))
            & expects
            $ Just A
          (A, B)
            & toListOf (takingWhile (_1 . iterateSucc) (< D))
            & expects [A, B, C]
          ([A .. E], B)
            & toListOf (droppingWhile (_1 . traverse) (< C))
            & expects [C, D, E]
          ([A .. E], B)
            & toListOf (dropping (_1 . traverse) 2)
            & expects [C, D, E]

          ([A .. E], B)
            & toListOf (droppingWhileBase2 @Int (_1 . traverse) (\_ v -> v < C))
            & expects [C, D, E]
          ([A .. E], B)
            & toListOf (droppingWhileBase2 @Int (_1 . traverse) (\i _ -> i < 3))
            & expects [D, E]

          -- case: take 0
          [1, 2 :: Int]
            & toListOf (takingWhile traverse (> 100))
            & expects []
          -- case: empty
          []
            & toListOf (takingWhile traverse (const True))
            & expects ([] :: [Int])

          -- case: non traversable
          A
            & preview (takingWhile id (< C))
            & expects
            $ Just A
          A
            & preview (takingWhile id (== Z))
            & expects
            $ Nothing
          A
            & view (takingWhile id (< C))
            & expects A
          A
            & view (takingWhile id (== Z))
            & expects mempty

        it "gets contravariant" $ do
          (1 :: Int)
            & toListOf (taking (replicated 5) 3)
            & expects [1, 1, 1]
        -- (1 :: Int) & toListOf (dropping (replicated 5) 3) & expects [1, 1]

        it "gets infinite" $ do
          (1 :: Int)
            & preview repeated
            & expects
            $ Just 1
          (1 :: Int)
            & toListOf (takingWhile (iterated (succ . succ)) (< 10))
            & expects [1, 3, 5, 7, 9]
          (1 :: Int)
            & toListOf (taking (iterated (succ . succ)) 4)
            & expects [1, 3, 5, 7]
          [1 :: Int ..]
            & toListOf (takingWhile traverse (< 10))
            & expects [1 .. 9]

          (1 :: Int)
            & toListOf (dropping (iterated (succ . succ)) 2)
            & take 3
            & expects [5, 7, 9]
          (1 :: Int)
            & toListOf (droppingWhile (iterated (succ . succ)) (< 10))
            & take 3
            & expects [11, 13, 15]

        it "folds" $ do
          [A ..]
            & preview (takingWhile folded (< D))
            & expects
            $ Just A
          [A ..]
            & toListOf (takingWhile folded (< D))
            & expects [A, B, C]
          [A ..]
            & toListOf (takingWhile folded (const False))
            & expects []
          [A ..]
            & preview (droppingWhile folded (< W))
            & expects
            $ Just W
          [A ..]
            & toListOf (droppingWhile folded (< W))
            & expects [W, X, Y, Z]
          [A ..]
            & toListOf (droppingWhile folded (const True))
            & expects []

        it "sets" $ do
          [A .. E]
            & set (takingWhile traverse (< C)) Z
            & expects [Z, Z, C, D, E]
          [A .. E]
            & over (takingWhile traverse (< C)) (succ . succ)
            & expects [C, D, C, D, E]
          [A .. E]
            & set (droppingWhile traverse (< C)) Z
            & expects [A, B, Z, Z, Z]
          [A .. E]
            & over (droppingWhile traverse (< C)) (succ . succ)
            & expects [A, B, E, F, G]

      it "[lined, worded]" $ do
        -- lined, worded
        let text =
              "hello world\n\
              \a\n \
              \b\n \
              \"
        text
          & toListOf lined
          & expects
          $ lines text
        text
          & toListOf worded
          & expects
          $ words text

    describe "Effects" $ do
      it "[Prelude]" $ do
        let x =
              [A .. E]
                & flip zip [1 :: Int ..]

        x
          & toIndexedList (traverse . _1)
          & expects
          $ [(0, A), (1, B), (2, C), (3, D), (4, E)]

        x
          & foldrOf (traverse . _1) (:) []
          & expects
          $ foldr (:) [] [A .. E]
        x
          & foldlOf (traverse . _1) (flip (:)) []
          & expects
          $ foldl (flip (:)) [] [A .. E]
        x
          & maxOf (traverse . _1)
          & expects E
        []
          & maxOf traverse
          & expects NotAlpha
        x
          & maxOf' (traverse . _1)
          & expects
          $ Just E
        []
          & maxOf' traverse
          & expects @(Maybe Alpha) Nothing

        x
          & elemOf (traverse . _1) C
          & expects True
        x
          & elemOf (traverse . _1) T
          & expects False
        A
          & nullOf repeated
          & expects False
        A
          & notNullOf repeated
          & expects True
        x
          & firstOf (traverse . _1)
          & expects
          $ Just A
        x
          & firstOf (traverse . _2)
          & expects
          $ Just 1
        x
          & lastOf (traverse . _1)
          & expects
          $ Just E
        x
          & lastOf (traverse . _2)
          & expects
          $ Just 5

      it "indexed" $ do
        let x =
              [A .. E]
                & flip zip [1 :: Int ..]

        x
          & itoListOf (folded . _1)
          & expects [(0, A), (1, B), (2, C), (3, D), (4, E)]
        x
          & itoListOf (folded . _2)
          & expects [(0, 1), (1, 2), (2, 3), (3, 4), (4, 5)]
        x
          & ifoldMapOf (folded . _1) (\i a -> show i <> show a <> ":")
          & expects "0A:1B:2C:3D:4E:"
        x
          & ifoldMapOf (folded . _2) (\i a -> show i <> show a <> ":")
          & expects "01:12:23:34:45:"

      it "[elemIndexOf] get index of element" $ do
        let x =
              [A .. E]
                & flip zip [1 :: Int ..]
        [A .. E]
          & elemIndexOf folded A
          & expects
          $ Just 0
        [A .. E]
          & elemIndexOf folded Z
          & expects
          $ (Nothing :: Maybe Int)
        x
          & elemIndexOf (folded . _1) Z
          & expects
          $ (Nothing :: Maybe Int)
        x
          & elemIndexOf (folded . _1) B
          & expects
          $ Just 1
        [A, B, C, A, D, E, A]
          & elemIndicesOf folded A
          & expects [0, 3, 6]
        [A, B, C, A, D, E]
          & elemIndicesOf folded Z
          & expects ([] @Int)

  describe "Lens.Traverse" $ do
    describe "Lenses" $ do
      it "[traversed] traverse with index" $ do
        [A .. E]
          & itoListOf traversed
          & expects [(0, A), (1, B), (2, C), (3, D), (4, E)]

      it "[both]" $ do
        -- get
        (A, B)
          & toListOf both
          & expects [A, B]

        -- set
        (A, B)
          & over both succ
          & expects (B, C)

      it "[beside]" $ do
        -- get
        (A, [B, C])
          & toListOf (beside id traverse)
          & expects [A, B, C]

        -- set
        (A, [B, C])
          & over (beside id traverse) fromEnum
          & expects (1, [2, 3])

        -- nested
        ((A, (B, C)), ((D, E), F))
          & toListOf (beside (beside id both) (beside both id))
          & expects [A, B, C, D, E, F]
        ((A, (B, C)), ((D, E), F))
          & over (beside (beside id both) (beside both id)) fromEnum
          & expects ((1, (2, 3)), ((4, 5), 6))

      it "[partsOf]" $ do
        Node (Leaf A) (Node (Leaf B) (Leaf C))
          & view (partsOf traverse)
          & expects [A, B, C]

        -- exact
        Node (Leaf A) (Node (Leaf B) (Leaf C))
          & set (partsOf traverse) [U, V, W]
          & expects (Node (Leaf U) (Node (Leaf V) (Leaf W)))

        -- too long
        Node (Leaf A) (Node (Leaf B) (Leaf C))
          & set (partsOf traverse) [U ..]
          & expects (Node (Leaf U) (Node (Leaf V) (Leaf W)))

        -- too short
        Node (Leaf A) (Node (Leaf B) (Leaf C))
          & set (partsOf traverse) [U]
          & expects (Node (Leaf U) (Node (Leaf B) (Leaf C)))

      it "[element, elements] get value at index" $ do
        let x =
              [A .. E]
                & flip zip [1 :: Int, 3 ..]

        -- element
        x
          & view (element 1 . _1)
          & expects
          $ B
        x
          & view (element 4 . _1)
          & expects
          $ E
        x
          & view (element 5 . _1)
          & expects
          $ mempty

        x
          & preview (element 0 . _2)
          & expects
          $ Just 1
        x
          & preview (element 1 . _2)
          & expects
          $ Just 3
        x
          & preview (element 4 . _2)
          & expects
          $ Just 9
        x
          & preview (element 5 . _2)
          & expects
          $ Nothing

        x
          & toListOf (elements (< 3) . _1)
          & expects [A, B, C]
        x
          & toListOf (elements (>= 3) . _1)
          & expects [D, E]
        x
          & toListOf (elements (< 3) . _2)
          & expects [1, 3, 5]
        x
          & toListOf (elements (>= 3) . _2)
          & expects [7, 9]
        -- case: element with infinite
        [1 :: Int ..]
          & preview (element 0)
          & expects
          $ Just 1

  describe "Lens.Set" $ do
    it "allows polymorphic update" $ do
      A
        & set id 'a'
        & expects 'a'
      (A, B)
        & set _1 'a'
        & expects ('a', B)

      A
        & over id show
        & expects "A"
      (A, B)
        & over _1 show
        & expects ("A", B)
      A
        & over id (replicate 3)
        & expects [A, A, A]

    it "sets" $ do
      (A, B)
        & set _1 Z
        & expects (Z, B)
      [A .. E]
        & over traverse succ
        & expects [B .. F]

      -- Setter
      (* 2)
        & set argument 10
        & ($ (1000 :: Integer))
        & expects (20 :: Int)
      (* 2)
        & set mapped 10
        & ($ (1000 :: Integer))
        & expects (10 :: Int)
      ((* 2), True)
        & set (_1 . argument) 10
        & fst
        & ($ (1000 :: Integer))
        & expects (20 :: Int)
      ((* 2), True)
        & set (_1 . mapped) 10
        & fst
        & ($ (1000 :: Integer))
        & expects (10 :: Int)

      -- Traversal
      [A .. D]
        & over traverse succ
        & expects [B, C, D, E]
      [A .. D]
        & over mapped succ
        & expects [B, C, D, E]

    describe "Lenses" $ do
      it "[sets] create setter" $ do
        let pairBoth = sets (\f (a, a') -> (f a, f a'))

        (A, B)
          & set _1 Z
          & expects (Z, B)
        (A, B)
          & set _2 Z
          & expects (A, Z)
        (A, B)
          & set pairBoth Z
          & expects (Z, Z)
        ((A, B), (C, D))
          & set (pairBoth . pairBoth) Z
          & expects ((Z, Z), (Z, Z))
        ((A, B), (C, D))
          & set (traverse . pairBoth) Z
          & expects ((A, B), (Z, Z))
        ((A, B), (C, D))
          & set (_1 . pairBoth) Z
          & expects ((Z, Z), (C, D))
        ((A, B), (C, D))
          & set (pairBoth . _1) Z
          & expects ((Z, B), (Z, D))

      it "[mapped, lifted] set functor/monad value" $ do
        let x =
              [A .. E]
                & flip zip [1 :: Int ..]

        x
          & set (mapped . _1) Z
          & expects [(Z, 1), (Z, 2), (Z, 3), (Z, 4), (Z, 5)]
        x
          & set (traverse . _1) Z
          & expects [(Z, 1), (Z, 2), (Z, 3), (Z, 4), (Z, 5)]
        x
          & set (lifted . _1) Z
          & expects [(Z, 1), (Z, 2), (Z, 3), (Z, 4), (Z, 5)]
        x
          & set (traverse . _1) Z
          & expects [(Z, 1), (Z, 2), (Z, 3), (Z, 4), (Z, 5)]

      it "[argument]" $ do
        let hello :: Show a => a -> String
            hello = ("Hello:" <>) . show

        hello
          & over argument (succ . succ)
          & over mapped ("Test " <>)
          & ($ A)
          & expects "Test Hello:C"

        hello
          & over (argument . _1) (succ . succ)
          & over (argument . _2) not
          & ($ (A, True))
          & expects "Hello:(C,False)"

        hello
          & over mapped (unwords . replicate 2)
          & over (mapped . mapped) toUpper
          & ($ A)
          & expects "HELLO:A HELLO:A"

    describe "Indexed" $ do
      it "[iset, iover]" $ do
        [A .. D]
          & iset traversed (< 2)
          & expects [True, True, False, False]
        [A .. D]
          & iover traversed (,)
          & expects [(0 :: Int, A), (1, B), (2, C), (3, D)]

    describe "Effects" $ do
      it "[set] set fixed value" $ do
        A
          & set id D
          & expects D

        -- composition
        (A, 'a')
          & set _1 B
          & expects (B, 'a')
        (A, 'a')
          & set _2 1
          & expects (A, 1 :: Int)
        [A, B, C]
          & set traverse B
          & expects [B, B, B]

      it "[over] map value" $ do
        A
          & over id succ
          & expects B
        A
          & over id show
          & expects "A"

        (A, 'a')
          & over _1 succ
          & expects (B, 'a')
        [A, B, C]
          & over traverse succ
          & expects [B, C, D]

      it "[Prelude]" $ do
        (1 :: Int)
          & id +~ 3
          & expects 4
        (1 :: Int, A)
          & _1 +~ 3
          & expects (4, A)
        [1 :: Int .. 5]
          & traverse +~ 3
          & expects [4, 5, 6, 7, 8]

        (1 :: Int, A)
          & _1 -~ 3
          & expects (-2, A)
        (1 :: Int, A)
          & _1 ?~ 4
          & expects (Just (4 :: Int), A)

      it "[State combinators] can code in imperative style" $ do
        let origin = Point 0 0

        ( do
            _startPoint .= Point 1 2
            _endPoint .= Point 3 4
          )
          & flip execState (Segment origin origin)
          & expects (Segment (Point 1 2) (Point 3 4))

        ( do
            (_startPoint . _coordX) += 10
            (_startPoint . _coordY) -= 20
            (_endPoint . _coordX) += 100
            (_endPoint . _coordY) -= 200
          )
          & flip execState (Segment origin origin)
          & expects (Segment (Point 10 (-20)) (Point 100 (-200)))

  describe "Lens.Prism" $ do
    let _positive :: Prism' Int Int
        _positive = nearly (> 0)
        _negative :: Prism' Int Int
        _negative = nearly (< 0)
        emptyString :: Prism' String String
        emptyString = nearly (== "")

    it "composes" $ do
      Just (Just (Left ((), Right True)))
        & preview (_Just . _Just . _Left . _2 . _Right)
        & expects (Just True)
      Just (Just (Left ((), Right @String True)))
        & preview (_Just . _Just . _Left . _2 . _Left)
        & expects Nothing
      Just (Just (Left ((), Right True)))
        & preview (_Just . _Just . (_Right @((), Either Int Bool) @String))
        & expects Nothing
      Just (Just (Left ((), Right True)))
        & preview (_Just . _Nothing)
        & expects Nothing
      Just (Just (Left ((), Right True)))
        & preview _Nothing
        & expects Nothing

    it "[Getter]" $ do
      Just A
        & preview _Just
        & expects (Just A)
      Just A
        & preview (_Just . to succ)
        & expects (Just B)

    it "[Traversal]" $ do
      [Left 'a', Right A, Left 'b', Right B]
        & itoListOf (indexing traverse . _Left)
        & expects [(0, 'a'), (2, 'b')]
      [Left 'a', Right A, Left 'b', Right B]
        & itoListOf (indexing traverse . _Right . to succ)
        & expects [(1, B), (3, C)]

    it "[Setter]" $ do
      (True, Right @Int A)
        & set (_2 . _Right) Z
        & expects (True, Right Z)
      (True, Left @Int 1)
        & set (_2 . _Right) Z
        & expects (True, Left 1)

    it "[Fold]" $ do
      [Left 'a', Right A, Right B, Right C, Left 'd']
        & foldOf (traverse . _Right)
        & expects C

    describe "Lenses" $ do
      it "[prism] build" $ do
        1 & isn't _positive & expects False
        (-1) & isn't _positive & expects True
        1 & set _positive 10 & expects 10
        (-1) & set _positive 10 & expects (-1)

        "" & isn't emptyString & expects False

      it "[_Just, _Nothing]" $ do
        [Just True, Nothing, Just False]
          & itoListOf (folded . _Just)
          & expects [(0, True), (2, False)]
        [Just True, Nothing, Just False]
          & itoListOf (folded . _Nothing)
          & expects [(1, ())]
        [Just True, Nothing, Just False]
          & itoListOf (folded . _Nothing . ito' (\i _ -> (i * 100, i)))
          & expects [(100, 1)]

      it "[_Left, _Right]" $ do
        [Right True, Left A, Right False, Left Z]
          & itoListOf (folded . _Right)
          & expects [(0, True), (2, False)]
        [Right True, Left A, Right False, Left Z]
          & itoListOf (folded . _Left)
          & expects [(1, A), (3, Z)]

      it "[_Show]" $ do
        "1"
          & preview _Show
          & expects (Just (1 :: Int))
        "1a"
          & preview _Show
          & expects (Just @Int 1)
        "a"
          & preview _Show
          & expects (Nothing @Int)
        "1"
          & preview _Show
          & expects (Just @Int 1)

      it "[outside] pattern matching" $ do
        let maybeP :: b -> (a -> b) -> Maybe a -> b
            maybeP b f =
              maybeP b f
                & set (outside _Nothing) (const b)
                & set (outside _Just) f
            eitherP :: (a -> r) -> (b -> r) -> Either a b -> r
            eitherP f g =
              eitherP f g
                & set (outside _Left) f
                & set (outside _Right) g
            tree :: forall a r. (a -> r) -> (Tree a -> Tree a -> r) -> Tree a -> r
            tree f g =
              tree f g
                & set (outside _Leaf) f
                & set (outside _Node) (uncurry g)

        Just B
          & maybeP Z id
          & expects B

        Nothing
          & maybeP Z id
          & expects Z

        Left A
          & eitherP succ pred
          & expects B
        Right A
          & eitherP succ pred
          & expects NotAlpha

        Leaf A
          & tree succ shouldNotHappen
          & expects B
        Node (Leaf C) (Leaf D)
          & tree succ (\l _ -> tree succ shouldNotHappen l)
          & expects D

      it "[aside] traverse coordinate" $ do
        ("one", 1)
          & set (aside _positive) ("ten", 10)
          & expects ("ten", 10)

        ("one", 1)
          & preview (aside _positive)
          & expects (Just ("one", 1))

        ("negative one", -1)
          & set (aside _positive) ("ten", 10)
          & expects ("negative one", -1)

        ("negative one", -1)
          & preview (aside _positive)
          & expects Nothing

      it "[without]" $ do
        let _isEOFError :: Prism' IOError IOError
            _isEOFError = nearly E.isEOFError
            e1 = E.ioeSetErrorType (E.userError "") E.eofErrorType

        Right @IOError @Int 10
          & over (without _isEOFError _negative) (Left . userError . either show (("Not positive " <>) . show))
          & expects (Right 10)
        Right @IOError @Int (-1)
          & over (without _isEOFError _negative) (Left . userError . either show (("Not positive " <>) . show))
          & expects (Left $ userError "Not positive -1")

        Left @IOError @Int e1
          & over (without _isEOFError _negative) (Left . userError . either show (("Not positive " <>) . show))
          & expects (Left $ userError "end of file")

        Right @IOError @Int 1
          & preview (without _isEOFError _positive)
          & expects (Just $ Right 1)
        Right @IOError @Int (-1)
          & preview (without _isEOFError _positive)
          & expects Nothing

        Left @IOError @Int (userError "ERROR")
          & preview (without _isEOFError _positive)
          & expects Nothing

        Left @IOError @Int e1
          & preview (without _isEOFError _positive)
          & expects (Just $ Left e1)

      it "[only, nearly]" $ do
        (1 :: Int)
          & preview (only 1)
          & expects (Just 1)
        (1 :: Int)
          & preview (only 0)
          & expects Nothing
        (1 :: Int)
          & preview (nearly (< 10))
          & expects (Just 1)
        (1 :: Int)
          & preview (nearly (< 1))
          & expects Nothing

        -- chain
        (1 :: Int)
          & preview (only 1 . to (+ 10))
          & expects (Just 11)
        (1 :: Int)
          & preview (only 0 . to (+ 10))
          & expects Nothing

      it "[prefix,suffix]" $ do
        "Hello world"
          & view (prefixed "Hello")
          & expects " world"
        "Hello world"
          & view (prefixed "world")
          & expects ""

        "Hello world"
          & view (suffixed "world")
          & expects "Hello "
        "Hello world"
          & view (suffixed "Hello")
          & expects ""

    describe "Effects" $ do
      it "[isn't]" $ do
        Nothing
          & isn't _Just
          & expects True
        Nothing
          & isn't _Nothing
          & expects False
        Just True
          & isn't _Just
          & expects False
        Just True
          & isn't _Nothing
          & expects True

        Right True
          & isn't _Right
          & expects False
        Right True
          & isn't _Left
          & expects True
        Left True
          & isn't _Right
          & expects True
        Left True
          & isn't _Left
          & expects False

      it "[matching]" $ do
        Just A
          & matching _Just
          & expects (Right @(Maybe Alpha) A)
        Just A
          & matching _Nothing
          & expects (Left (Just A))
        Nothing
          & matching _Nothing
          & expects (Right @(Maybe Alpha) ())
        Nothing
          & matching _Just
          & expects (Left @(Maybe Alpha) @Alpha Nothing)

        Left A
          & matching _Left
          & expects (Right @(Either Alpha Int) A)
        Left A
          & matching _Right
          & expects (Left @(Either Alpha Int) @Int (Left A))

      it "[below]" $ do
        [Just A, Just B, Nothing]
          & view (below _Just)
          & expects []
        [Just A, Just B]
          & view (below _Just)
          & expects [A, B]
        [Nothing, Nothing]
          & view (below _Nothing)
          & expects [(), ()]

        -- with index
        [Just A, Just B]
          & itoListOf (below _Just . folded)
          & expects [(0, A), (1, B)]
        [Nothing, Nothing]
          & itoListOf (below _Nothing . folded)
          & expects [(0, ()), (1, ())]

  it "scratch" $ do
    [True]
      & toListOf (traverse . to show)
      & expects ["True"]
    A
      & view (to show)
      & expects "A"
    (1 :: Int)
      & view (to show)
      & expects "1"

    putStrLn "== preview ==="
    [1 .. 10 :: Int]
      & preview (takingWhile traverse (< 5))
      & expects (Just 1)
    putStrLn "== list ==="
    [1 .. 10 :: Int]
      & toListOf (takingWhile traverse (< 5))
      & expects [1, 2, 3, 4]
    putStrLn "== set ==="
    [1 .. 10 :: Int]
      & set (takingWhile traverse (< 3)) 11
      & expects [11, 11, 3, 4, 5, 6, 7, 8, 9, 10]
    putStrLn "== take infinite ==="
    [1 :: Int ..]
      & toListOf (takingWhile traverse (< 5))
      & expects [1, 2, 3, 4]
    putStrLn "== take iterated ==="
    (1 :: Int)
      & toListOf (takingWhile (iterated (succ . succ)) (< 10))
      & expects [1, 3, 5, 7, 9]

    print $ deferAp [1 :: Int .. 10]
