{-# LANGUAGE RecursiveDo #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant flip" #-}
{-# HLINT ignore "Use replicate" #-}
{-# HLINT ignore "Use <&>" #-}
module TestMonad
  ( testMonad,
    testMonadFix,
    testWrappedMonad,
    testCompositionMonad,
    testFunctorMonad,
    testMonadFail,
    testStaticArrow,
  )
where

import Control.Applicative
import Control.Arrow
import Control.Monad
import Control.Monad.Fix
import Data.Char
import Data.Foldable
import Data.Functor.Identity
import Data.Functor.Sum
import Data.Maybe
import StaticArrow
import TestUtils

testCompositionMonad =
  callTest
    ( do
        let f = (* 2)
        let g = (*) . (+ 3) -- f(x,y) = y(x+3)

        -- test g <*> f == f >>= flip g
        let x = g <*> f $ 2
        let y = (f >>= flip g) 2
        let z = (f <**> g) 2
        assertIO (x == y) $ show x
        assertIO (x == z) $ show x

        let x = g <*> f $ 5
        let y = (f >>= flip g) 5
        let z = (f <**> g) 5
        assertIO (x == y) $ show x
        assertIO (x == z) $ show x
    )
    "testCompositionMonad"

testArrow =
  callTest
    ( do
        -- Basic
        let f = (+ 4) >>> (* 3)
        print $ f 1
        print $ f 2

        -- Conditional
        let f :: Either Int Char -> Either Int Char
            f = left (* 3)
        let g :: Either Int Char -> Either Int Char
            g = right $ chr . (+ 1) . ord
        print $ f $ Left 1
        print $ f $ Right 'a'
        print $ g $ Left 1
        print $ g $ Right 'a'

        let f = (+ 2) +++ (+ 3)
        print $ f $ Left 1
        print $ f $ Right 1

        let f = (+ 2) ||| ord
        print $ f $ Left 1
        print $ f $ Right 'a'
        testDone
    )
    "testArrow"

testCaseExpression =
  callTest
    ( do
        let f x = case x of
              0 : _ -> "zero"
              1 : _ -> "one"
              _ -> "other"
        let g x = case x of
              [f]
                | f == 0 -> "0"
                | f == 1 -> "1"
              [_, g]
                | g == 0 -> "00"
                | g == 1 -> "01"
                | otherwise -> "NA g"
              _ -> show x
        print $ map (\x -> f [x]) [0 .. 10]
        print $ map (\x -> f [-1, x]) [0 .. 10]
        print $ map (\x -> g [x]) [0 .. 10]
        print $ map (\x -> g [-1, x]) [0 .. 10]
        testDone
    )
    "testCaseExpression"

testWrappedMonad =
  callTest
    ( do
        let x = (WrapMonad $ Just 1) :: WrappedMonad Maybe Int
        let x = WrapMonad [1] :: WrappedMonad [] Int
        let f = WrapArrow (* 2)
        print $ (unwrapArrow $ (+) <$> f <*> f) 2
        print $ unwrapMonad $ (* 2) <$> x
        testDone
    )
    "testWrappedMonad"

instance Num a => MonadFail (Either a) where
  fail _ = Left (-1)

testMonadFail =
  callTest
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
              (v : xs) <- x
              Right (2 * v)
        let doubleFirstItemWithoutMonadFail :: Num a => Either a [a] -> Either a a
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

testFunctorMonad =
  callTest
    ( do
        let a = Identity 1
        let l = InL $ Just 1
        let r = InR $ Just 2

        let printSum = traverse_ print
        printSum l
        printSum r
        testDone
    )
    "testFunctorMonad"

testMonad =
  callTest
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

testMonadFix =
  callTest
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

            repeatN_raw n = n : repeatN_raw n
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

            count_fixBad :: [Int]
            -- BAD. Cannot access fixed point
            -- count_fixBad = fix (\list -> (head list + 1):list)
            count_fixBad = fix (\list@(x : rest) -> (x + 1) : list)

            countInMonadFrom :: MonadFix m => m (Int -> [Int])
            countInMonadFrom = mfix (\next -> return (\n -> n : next (n + 1)))
            countInMonadFrom0 :: MonadFix m => m [Int]
            countInMonadFrom0 = countInMonadFrom <*> return 0

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

            replExec :: (Bool -> String -> IO ()) -> Bool -> String -> IO ()
            replExec runRepl isFirstCall input = do
              let queryNext = do
                    putStrLn "Enter next value. Type 'q' to exit"
                    nextInput <- getLine
                    runRepl False nextInput
                  result
                    -- case: quit
                    | input == "q" || input == "y"
                        = do putStrLn "done"
                    -- case: initial input
                    | isFirstCall = do
                        putStrLn "Start REPL"
                        queryNext
                    | otherwise = do
                        unless (null input) (putStrLn $ "value:" ++ input)
                        queryNext
               in result
            repl_fix :: Bool -> String -> IO ()
            repl_fix = replExec repl_fix
            repl_mfix :: MonadFix m => m (Bool -> String -> IO ())
            repl_mfix = mfix $ return.replExec
            repl_dorec :: MonadFix m => m (Bool -> String -> IO ())
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

        printBanner "range"
        print [0 .. 10]
        print $ rangeRaw 0 10
        print $ do f <- range_dorec; Identity (f 0 10)
        print $ do f <- range_dorec; Just (f 0 10)
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

        do printBanner "REPL fix"; repl_fix True ""
        do printBanner "REPL mfix"; f <- repl_mfix; f True ""
        do printBanner "REPL dorec"; f <- repl_dorec; f True ""
        printBanner "REPL plain"
        let
          -- BAD:
          -- repl = repl >>= return. replExec
          -- in repl >>= (\f -> f True "")
          repl = replExec repl
         in do repl True ""

        testDone
    )
    "testMonadFix"

testStaticArrow =
  callTest
    ( do
        printBanner "StaticArrow"
        let f = StaticArrow $ Just (* 2)
        let g = StaticArrow $ Just (+ 10)
        let h = f >>> g

        -- 2x+10
        let fn = getStaticArray h
        print $ fn <*> pure 1
        print $ traverse ((fn <*>) . pure) [1 .. 10]

        let fn = getStaticArray (first f >>> second g)
        let dup x = (x, x)
        print $ fn <*> pure (1, 2)
        print $ traverse ((fn <*>) . pure . dup) [1 .. 10]

        printBanner "WrapArrow"
        let wf = WrapArrow (* 2)
        let wg = WrapArrow (+ 10)
        let wh = wf >>> wg
        print $ unwrapArrow wf 2
        print $ unwrapArrow wh 2
        testDone
    )
    "testStaticArrow"
