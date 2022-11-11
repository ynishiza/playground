{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant flip" #-}
{-# HLINT ignore "Use replicate" #-}
{-# HLINT ignore "Use <&>" #-}
{-# LANGUAGE RecursiveDo #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module TestMonad
  ( testMonad,
    testMonadFix,
    testWrappedMonad,
    testCompositionMonad,
    testFunctorMonad,
    testMonadFail,
    testStaticArrow,
    allTests
  )
where

import Control.Applicative
import Control.Arrow
import Control.Monad
import Control.Monad.Fix
import Data.Foldable
import Data.Functor.Identity
import Data.Functor.Sum
import Data.Maybe
import StaticArrow
import TestUtils

allTests :: TestState
allTests = wrapTest (do
  testMonad
  testCompositionMonad
  testWrappedMonad
  testMonadFail
  testFunctorMonad
  testMonadFix
  testStaticArrow
                  ) "TestMonad"

testCompositionMonad :: TestState
testCompositionMonad =
  createTest
    ( do
        let f = (* 2)
        let g = (*) . (+ 3) -- f(x,y) = y(x+3)

        -- test g <*> f == f >>= flip g
        let 
          x = g <*> f $ 2
          y = (f >>= flip g) 2
          z = (f <**> g) 2
        assertIO (x == y) $ show x
        assertIO (x == z) $ show x

        let 
          x1 = g <*> f $ 5
          y1 = (f >>= flip g) 5
          z1 = (f <**> g) 5
        assertIO (x1 == y1) $ show x1
        assertIO (x1 == z1) $ show x1
    )
    "testCompositionMonad"

testWrappedMonad :: TestState
testWrappedMonad =
  createTest
    ( do
        let 
          x = (WrapMonad $ Just 1) :: WrappedMonad Maybe Int
          _ = WrapMonad [1] :: WrappedMonad [] Int
          f = WrapArrow (* 2)
        print $ (unwrapArrow $ (+) <$> f <*> f) 2
        print $ unwrapMonad $ (* 2) <$> x
        testDone
    )
    "testWrappedMonad"

instance Num a => MonadFail (Either a) where
  fail _ = Left (-1)

testMonadFail :: TestState
testMonadFail =
  createTest
    ( do
        printBanner "case: with builtin MonadFail(Maybe)"
        let doubleMaybe x = do
              (Right v) <- x
              Just (2 * v)
        print $ doubleMaybe $ Just (Right 2)
        print $ doubleMaybe $ Just (Left 2)

        printBanner "case:with custom Num a => MonadFail(Either a)"
        let doubleFirstItem :: Num a => Either a [a] -> Either a a
            doubleFirstItem x = do
              (v : _) <- x
              Right (2 * v)
            doubleFirstItemWithoutMonadFail :: Num a => Either a [a] -> Either a a
            doubleFirstItemWithoutMonadFail x = do
              list <- x
              let v
                    | (a : _) <- list = Right (2 * a)
                    | otherwise = Left (-1)
               in v
        print $ doubleFirstItem (Right [])
        print $ doubleFirstItem (Right [1])
        print $ doubleFirstItemWithoutMonadFail (Right [])
        print $ doubleFirstItemWithoutMonadFail (Right [1])

        printBanner "case: with no MondaFail(Either String)"
        let g2 :: Either Char (Maybe Char) -> Either Char Char
            g2 x = do
              -- NO. Doesn't work without MonadFail
              -- Just v <- x
              -- Right (v ++ "test")

              n <- x
              Right $ fromMaybe '\NUL' n
        print $ g2 (Right Nothing)
        print $ g2 (Right (Just 'a'))
        print $ g2 (Right (Just 'b'))
        testDone
    )
    "testMonadFail"

testFunctorMonad :: TestState
testFunctorMonad =
  createTest
    ( do
        let _ = Identity 1
        let l = InL $ Just 1
        let r = InR $ Just 2

        let printSum = traverse_ print
        printSum l
        printSum r
        testDone
    )
    "testFunctorMonad"

testMonad :: TestState
testMonad =
  createTest
    ( do
        printBanner "do IO"
        do
          putStr "Hello"
          putStr "there"

        do
          putStr "Enter value"
          x <- getLine
          let msg = "message" ++ x
          putStr msg

        printBanner "do ->,[]"
        let f = do
              t <- (+ 10)
              (+ (2 * t))
        print $ f 3
        let x = do
              t <- [1 .. 10]
              [1, 2 .. t]
        print x

        testDone
    )
    "testMonad"

type REPLInput = String

testMonadFix :: TestState
testMonadFix =
  createTest
    ( do
        let repeat_raw :: [Int]
            repeat_raw = 1 : repeat_raw
            repeat_fix :: [Int]
            repeat_fix = fix (1 :)
            repeat_mfix :: MonadFix m => m [Int]
            repeat_mfix = mfix (\list -> return (1 : list))
            repeat_dorec :: MonadFix m => m [Int]
            repeat_dorec = do
              rec list <- return (1 : list)
              return list

            repeatN_fix :: a -> [a]
            repeatN_fix n = fix (n :)

            repeatN_mfix :: MonadFix m => Int -> m [Int]
            repeatN_mfix n = mfix (\list -> do return (n : list))
            repeatN_dorec :: MonadFix m => Int -> m [Int]
            repeatN_dorec n = do
              rec list <- return (n : list)
              return list

            rangeRaw :: Int -> Int -> [Int]
            rangeRaw start end
              | start > end = []
              | otherwise = start : rangeRaw (succ start) end
            range_fix :: Int -> Int -> [Int]
            range_fix =
              fix
                ( \range start end ->
                    let x
                          | start > end = []
                          | otherwise = start : range (succ start) end
                     in x
                )
            range_mfix :: MonadFix m => m (Int -> Int -> [Int])
            range_mfix =
              mfix
                ( \range ->
                    return
                      ( \start end ->
                          let x
                                | start > end = []
                                | otherwise = start : range (succ start) end
                           in x
                      )
                )
            range_dorec :: MonadFix m => m (Int -> Int -> [Int])
            range_dorec = do
              rec range <-
                    return
                      ( \start end ->
                          let x
                                | start > end = []
                                | otherwise = start : range (succ start) end
                           in x
                      )
              return range

            enumFrom_raw n = n : enumFrom_raw (n + 1)
            enumFrom_fix :: Int -> [Int]
            enumFrom_fix = fix (\next n -> n : next (n + 1))
            enumFrom0_fix = enumFrom_fix 0
            enumFrom_mfix :: MonadFix m => m (Int -> [Int])
            enumFrom_mfix = mfix (\next -> return (\n -> n : next (n + 1)))

            -- count_fixBad :: [Int]
            -- BAD. Cannot access fixed point
            -- count_fixBad = fix (\list -> (head list + 1):list)
            -- count_fixBad = fix (\list@(x : _) -> (x + 1) : list)

            factorial_mfix :: MonadFix m => m (Int -> Int)
            factorial_mfix =
              mfix
                ( \f ->
                    return (\n -> if n <= 1 then n else n * f (n - 1))
                )
            factorial_dorec :: MonadFix m => m (Int -> Int)
            factorial_dorec = do
              rec f <- return (\n -> if n <= 1 then n else n * f (n - 1))
              return f

            replExec :: (Int -> REPLInput -> IO ()) -> Int -> REPLInput -> IO ()
            replExec runRepl callCount input = do
              let queryNext = do
                    putStrLn "Enter next value. Type 'q' to exit"
                    nextInput <- getLine
                    runRepl (callCount + 1) nextInput
                  result
                    -- case: quit
                    | input == "q" || input == "y"
                        = do putStrLn "done"
                    -- case: initial input
                    | callCount == 0 = do
                        putStrLn "Start REPL"
                        queryNext
                    | callCount == 3 = do
                        do putStrLn "Aborting."
                    | otherwise = do
                        unless (null input) (putStrLn $ "value:" ++ input)
                        queryNext
               in result
            repl_fix :: Int -> REPLInput -> IO ()
            repl_fix = replExec repl_fix
            repl_mfix :: MonadFix m => m (Int -> REPLInput -> IO ())
            repl_mfix = mfix $ return.replExec
            repl_dorec :: MonadFix m => m (Int -> String -> IO ())
            repl_dorec = do
              rec runRepl <- return (replExec runRepl)
              return runRepl

        printBanner "repeat"
        print $ take 10 $ repeat 1
        print $ take 10 repeat_fix
        print $ take 10 repeat_raw
        print $ do l <- repeat_dorec; Identity (head l)
        print $ do l <- repeat_dorec; Identity (take 10 l)
        print $ do l <- repeat_mfix; Identity (take 10 l)
        print $ take 10 $ repeatN_fix 2
        print $ do l <- repeatN_mfix 2; Identity (take 10 l)
        print $ do l <- repeatN_mfix 2; Just (take 10 l)
        print $ do l <- repeatN_dorec 2; Just (take 10 l)

        printBanner "range"
        print [0 .. 10]
        print $ rangeRaw 0 10
        print $ do f <- range_dorec; Identity (f 0 10)
        print $ do f <- range_dorec; Just (f 0 10)
        print $ do f <- range_mfix; Identity (f 0 10)
        print [(-10) .. 10]
        print $ rangeRaw (-10) 10
        print $ range_fix (-10) 10
        print $ do f <- range_dorec; Just (f (-10) 10)

        printBanner "enum"
        print $ take 10 enumFrom0_fix
        print $ take 10 $ enumFrom_raw 0
        print $ do f <- enumFrom_mfix; Just (take 10 $ f 0)

        printBanner "Factorial"
        print $ do f <- factorial_dorec; Just (f 5)
        print $ do f <- factorial_mfix; Just (f 5)

        do printBanner "REPL fix"; repl_fix 0 ""
        do printBanner "REPL mfix"; f <- repl_mfix; f 0 ""
        do printBanner "REPL dorec"; f <- repl_dorec; f 0 ""
        printBanner "REPL plain"
        let
          -- BAD:
          -- repl = repl >>= return. replExec
          -- in repl >>= (\f -> f True "")
          repl = replExec repl
         in do repl 0 ""

        testDone
    )
    "testMonadFix"

testStaticArrow :: TestState
testStaticArrow =
  createTest
    ( do
        printBanner "StaticArrow"
        let f = StaticArrow $ Just (* 2)
        let g = StaticArrow $ Just (+ 10)
        let h = f >>> g

        -- 2x+10
        let fn = getStaticArray h
        print $ fn <*> pure 1
        print $ traverse ((fn <*>) . pure) [1 .. 10]

        let fn2 = getStaticArray (first f >>> second g)
        let dup x = (x, x)
        print $ fn2 <*> pure (1, 2)
        print $ traverse ((fn2 <*>) . pure . dup) [1 .. 10]

        printBanner "WrapArrow"
        let wf = WrapArrow (* 2)
        let wg = WrapArrow (+ 10)
        let wh = wf >>> wg
        print $ unwrapArrow wf 2
        print $ unwrapArrow wh 2
        testDone
    )
    "testStaticArrow"
