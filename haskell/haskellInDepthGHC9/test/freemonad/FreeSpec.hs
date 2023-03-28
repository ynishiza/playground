{-# LANGUAGE DerivingStrategies #-}

module FreeSpec
  ( spec,
  )
where

import Control.Monad
import Control.Monad.Trans
import Data.Foldable
import Data.Function
import Data.Functor.Classes
import F (F (..), liftPure)
import F qualified
import Free (Free (..))
import Free qualified
import List hiding (test)
import System.IO.Extra
import Test.Hspec

spec :: Spec
spec = describe "" $ do
  freeSpec
  fSpec

data A a = A a | B a
  deriving stock (Show, Functor)

instance Show1 A where
  liftShowsPrec s _ i (A a) = ("A " <>) . showParen True (s i a)
  liftShowsPrec s _ i (B a) = ("B " <>) . showParen True (s i a)

show1 :: (Show1 f, Show a) => f a -> String
show1 = ($ "") . showsPrec1 1

expects :: (Show a, Eq a) => a -> a -> IO ()
expects = flip shouldBe

freeSpec :: Spec
freeSpec = describe "Free" $ do
  it "basic" $ do
    let addN n = (+ n)
    ( lift . addN
        >=> lift . addN
        >=> lift . addN
      )
      & ($ 10)
      & Free.foldFree id
      & ($ (1 :: Int))
      & expects 13

    ( lift . addN
        >=> lift . addN
        >=> lift . addN
        >=> lift . addN
        >=> lift . addN
      )
      & ($ 10)
      & Free.foldFree id
      & ($ (1 :: Int))
      & expects 15

  it "appends" $ do
    Free.liftF (A True)
      & show1
      & expects "Free (A (Pure True))"
    ( Free.liftF (A True)
        >>= Free.liftF . B
      )
      & show1
      & expects "Free (A (Free (B (Pure True))))"
    ( Free.liftF (A True)
        >>= Free.liftF . B
        >>= Free.liftF . B
      )
      & show1
      & expects "Free (A (Free (B (Free (B (Pure True))))))"
    ( Free.liftF (A True)
        >>= Free.liftF . B
        >>= Free.liftF . B
        >>= Free.liftF . A
      )
      & show1
      & expects "Free (A (Free (B (Free (B (Free (A (Pure True))))))))"

  it "cutsoff" $ do
    let value :: Free IO ()
        value =
          foldr
            (\a r -> Free.liftF (putStrLn [a]) >> r)
            (pure ())
            "abcd"

        test expected io =
          captureOutput (Free.foldFree id io)
            >>= expects expected

    Free.cutoff 0 value
      & test ("", Nothing)
    Free.cutoff 1 value
      & test ("a\n", Nothing)
    Free.cutoff 2 value
      & test ("a\nb\n", Nothing)
    Free.cutoff 3 value
      & test ("a\nb\nc\n", Nothing)
    Free.cutoff 4 value
      & test ("a\nb\nc\nd\n", Nothing)
    Free.cutoff 5 value
      & test ("a\nb\nc\nd\n", Just ())
    Free.cutoff 100 value
      & test ("a\nb\nc\nd\n", Just ())

fSpec :: Spec
fSpec = describe "F" $ do
  it "liftPure" $ do
    execListF (liftPure @Int @(List Int) 100)
      & expects ([], 100)

  it "liftF" $ do
    execListF (liftNilF @Int)
      & expects ([], ())
    execListF (liftConsF @Int 1)
      & expects ([1], ())

  it "binds" $ do
    execListF
      ( do
          liftConsF (1 :: Int)
          liftConsF 2
          liftConsF 3
          liftConsF 10
      )
      & expects ([1, 2, 3, 10], ())

  describe "cutoff" $ do
    it "should cutoff" $ do
      let list = [1 .. 10 :: Int]
          freeList :: F (List Int) ()
          freeList = liftListF list
          testCutoff fl l n =
            ( execListF (F.cutoff n fl)
                & expects (take n l, Nothing)
            )
              >> ( execListF (F.cutoff' n fl)
                     & expects (take n l, Nothing)
                 )
      traverse_ (testCutoff freeList list) list
