{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnboxedTuples #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}

{-# HLINT ignore "Use uncurry" #-}

module Chapter9_2
  ( run,
    testStrictVariantFunctions,
    testStrcitWithSum,
    testStrictWithFibonacci,
    testUnpacking,
    sumN,
    sumNBang,
    sumNSeq,
    fibBase,
    fibFast,
    fibSeq,
    fibBang,
    fibUnpacked,
  )
where

import Control.Exception
import Criterion.Main
import Criterion.Types
import Data.Foldable
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Fmt
import Utils

bt :: a
bt = undefined

run :: TestState
run =
  wrapTest
    ( do
        test
        testStrictVariantFunctions
        testStrictWithFibonacci
        testStrcitWithSum
        testUnpacking
    )
    "Chpater 9"

test :: TestState
test =
  createChapterTest
    "9.2"
    "Weak head normal form"
    ( do
        testBoth "bottom" bt assertFalse assertFalse
        testBoth "undefined * 2" (bt * 2) assertFalse assertFalse
        testBoth "2 * undefined" (bt * 2) assertFalse assertFalse
        testBoth "undefined <$> Just 1" (undefined <$> Just 1) assertSuccess assertSuccess
        testBoth "undefined <*> Just 1" (undefined <*> Just 1) assertFalse assertFalse
        testBoth "Just 1 >>= undefined" (Just 1 >>= undefined) assertFalse assertFalse

        testBoth "Just undefined" (Just bt) assertSuccess assertSuccess

        testDone
    )
  where
    testBoth m x s t = do
      _ <- testBang m x >>= s
      _ <- testSeq m x >>= t
      return ()
    assertSuccess = assertIsEqual True . snd
    assertFalse = assertIsEqual False . snd

data Showable = forall a. Show a => Showable a

instance Show Showable where
  show (Showable x) = show x

sumN :: Integer -> Integer
sumN = f 0
  where
    f v0 0 = v0
    f v0 n = f (v0 + n) (n -1)

sumNAp :: Integer -> Integer
sumNAp = f 0
  where
    f v0 0 = v0
    -- f v0 n = flip f (n - 1) $! (v0 + n)
    f v0 n = flip f (n - 1) $! (v0 + n)

sumNSeq :: Integer -> Integer
sumNSeq = f 0
  where
    f v0 0 = v0
    f v0 n = let v' = v0 + n in v' `seq` f v' (n -1)

sumNBang :: Integer -> Integer
sumNBang = f 0
  where
    f v0 0 = v0
    f v0 n = let !v' = v0 + n in f v' (n -1)

testStrcitWithSum :: TestState
testStrcitWithSum =
  createChapterTest
    "9.2"
    "testStrcitWithSum"
    ( do
        let x0 = 10 ^ 6
         in do
              defaultMainWith
                (defaultConfig {reportFile = Just "bench.sumN.html", timeLimit = 2})
                [ bench "sumN" (whnf sumN x0),
                  bench "sumNAp" (whnf sumNAp x0),
                  bench "sumNSeq" (whnf sumNSeq x0),
                  bench "sumNBang" (whnf sumNBang x0)
                ]
        testDone
    )

fibBase :: Integer -> Integer
fibBase 0 = 0
fibBase 1 = 1
fibBase n = fibBase (n - 1) + fibBase (n - 2)

fibFast :: Integer -> Integer
fibFast 0 = 0
fibFast 1 = 1
fibFast n0 = fst r + snd r
  where
    r = g 2 (1, 0)
    g n v@(fn1, fn2)
      | n == n0 = v
      | otherwise = let f' = fn1 + fn2 in g (n + 1) (f', fn1)

fibSeq :: Integer -> Integer
fibSeq 0 = 0
fibSeq 1 = 1
fibSeq n0 = fst r + snd r
  where
    r = g 2 (1, 0)
    g n v@(fn1, fn2)
      | n == n0 = v
      | otherwise = let f' = fn1 + fn2 in seq f' $ g (n + 1) (f', fn1)

fibBang :: Integer -> Integer
fibBang 0 = 0
fibBang 1 = 1
fibBang n0 = fst r + snd r
  where
    r = g 2 (1, 0)
    g n v@(!fn1, fn2)
      | n == n0 = v
      | otherwise = let f' = fn1 + fn2 in g (n + 1) (f', fn1)

fibUnpacked :: Integer -> Integer
fibUnpacked 0 = 0
fibUnpacked 1 = 1
fibUnpacked n0 = v1 + v2
  where
    (# v1, v2 #) = g 2 (# 1, 0 #)
    g n v@(# !fn1, fn2 #)
      | n == n0 = v
      | otherwise = let f' = fn1 + fn2 in g (n + 1) (# f', fn1 #)

testStrictWithFibonacci :: TestState
testStrictWithFibonacci =
  createChapterTest
    "9.2"
    "testStrictWithFibonacci"
    ( do
        let assertFib f x = do
              assertIsEqual (fibBase x) (f x)
         in do
              traverse_ (assertFib fibFast) [1 .. 20]
              traverse_ (assertFib fibSeq) [1 .. 20]
              traverse_ (assertFib fibBang) [1 .. 20]
              traverse_ (assertFib fibUnpacked) [1 .. 20]
        let x1 = 10 ^ 5
         in do
              defaultMainWith
                (defaultConfig {timeLimit = 2, reportFile = Just "bench.Fibonacci.html"})
                [ bench "fibFast" (whnf fibFast x1),
                  bench "fibSeq" (whnf fibSeq x1),
                  bench "fibBang" (whnf fibBang x1),
                  bench "fibUnpacked" (whnf fibUnpacked x1)
                ]
        testDone
    )

testStrictVariantFunctions :: TestState
testStrictVariantFunctions =
  createChapterTest
    "9.2"
    "testStrictVariantFunctions e.g. foldr', foldl'"
    ( do
        let x0 :: [Int]
            x0 = [1 .. 10 ^ 6]
            f x = ((2 * x) :)
            f' = flip f
         in do
              defaultMain
                [ bench "list foldl'" $ whnf (length . foldl' f' []) x0,
                  bench "list foldr'" $ whnf (length . foldr' f []) x0,
                  -- bench "list foldl" $ whnf (last . foldl ap []) x,
                  -- bench "list foldr" $ whnf (last . foldr (:) []) x,

                  bench "sum foldl'" $ whnf (foldl' (+) 0) x0,
                  bench "sum foldr'" $ whnf (foldr' (+) 0) x0
                  -- bench "sum foldl" $ whnf (foldr' (+) 0) x,
                  -- bench "sum foldr" $ whnf (foldr' (+) 0) x
                ]

        let !z = 1
            (!x : (!xs)) = [1 .. 10]
            [!a0, !a1, !a2] = [1, 2, 3]
            (!a, !b) = (1, 2)
            Just !y = Just 1

            f !x = undefined
            g (!x, !y) = undefined
            h (!x : (!xs)) = undefined
         in print
              [ Showable z,
                Showable x,
                Showable xs,
                Showable a,
                Showable b,
                Showable y,
                Showable a0,
                Showable a1,
                Showable a2,
                Showable ()
              ]
        testDone
    )

testBang :: T.Text -> a -> IO (T.Text, Bool)
testBang expr x = testStrict ("! (" +| expr |+ ")") $ let !_ = x in return ()

testSeq :: T.Text -> a -> IO (T.Text, Bool)
testSeq expr x = testStrict ("seq " +|| expr ||+ "") $ return $ seq x ()

testStrict :: Show a => T.Text -> IO a -> IO (T.Text, Bool)
testStrict label io =
  ( do
      v <- io
      fmtLn ("OK " +| label |+ "" <> indentF 1 (build (show v)))
      return (label, True)
  )
    `catch` f
  where
    f :: SomeException -> IO (T.Text, Bool)
    f e =
      let msg = fmt $ nameF "ERROR" (build label) <> indentF 1 (build $ show e)
       in T.putStrLn msg >> return (msg, False)

data MyPoint = MyPoint !Int !Int deriving (Show)

data MyUnpackedPoint = MyUnpackedPoint {x :: {-# UNPACK #-} !Int, y :: {-# UNPACK #-} !Int}
  deriving (Show, Eq)

data MyUnpackedPoint2 = MyUnpackedPoint2 {-# UNPACK #-} (Int, Int)

testUnpacking :: TestState
testUnpacking =
  createChapterTest
    "9.2.2"
    "testUnpacking"
    ( do
        print $ MyUnpackedPoint 100 20
        print $ MyPoint 100 20

        let toTuple :: forall a b. a -> b -> (# a, b #)
            toTuple x y = (# x, y #)
            toTriple :: forall a b c. a -> b -> c -> (# a, b, c #)
            toTriple x y z = (# x, y, z #)
            nonNegative :: Double -> (# Double| String #)
            nonNegative x
              | x < 0 = (# | "Negative" #)
              | otherwise = (# x | #)

            printTuple (# v1, v2 #) = fmtLn $ "(# " +|| v1 ||+ "," +|| v2 ||+ " #)"
            printTriple (# v1, v2, v3 #) = fmtLn $ "(# " +|| v1 ||+ "," +|| v2 ||+ "," +|| v3 ||+ " #)"
            printSum (# v1 | #) = fmtLn $ "(# " +|| v1 ||+ " | #)"
            printSum (# | v2 #) = fmtLn $ "(# |" +|| v2 ||+ " #)"
         in do
              printTuple (toTuple 1 2)
              printTriple (toTriple 1 2 3)
              printSum (nonNegative (-1))
              printSum (nonNegative 1)
        testDone
    )
