module TestArrow
  ( testArrow,
    testArrowLoop,
    allTests,
  )
where

import Control.Arrow
import Data.Char
import Data.Function
import Data.List (partition)
import TestUtils

allTests :: TestState
allTests =
  wrapTest
    ( do
        testArrow
        testArrowLoop
    )
    "TestArrow"

testArrow :: TestState
testArrow =
  createTest
    ( do
        -- Basic
        let f = (+ 4) >>> (* 3)
        print $ f 1
        print $ f 2

        -- Conditional
        let g :: Either Int Char -> Either Int Char
            g = left (* 3)
            h :: Either Int Char -> Either Int Char
            h = right $ chr . (+ 1) . ord
        print $ g $ Left 1
        print $ g $ Right 'a'
        print $ h $ Left 1
        print $ h $ Right 'a'

        let f2 = (+ 2) +++ (+ 3)
        print $ f2 $ Left 1
        print $ f2 $ Right 1

        let f3 = (+ 2) ||| ord
        print $ f3 $ Left 1
        print $ f3 $ Right 'a'
        testDone
    )
    "testArrow"

testArrowLoop :: TestState
testArrowLoop =
  createTest
    ( do
        -- loop
        printBanner "Loop test"

        let -- an equivalent implementation of loop
            loop2 f x =
              let f1 = fst . f
                  f2 = snd . f
               in f1 (x, fix (\b -> f2 (x, b)))

            -- factorial
            factorialFix :: Integral a => (a -> a) -> a -> a
            factorialFix fact n = if n <= 1 then n else n * fact (n - 1)
            factorialLoop :: Integral a => (a, a -> a) -> (a, a -> a)
            factorialLoop (n, f) = (f n, factorialFix f)
            factorialLoop2 (n, f) = (f n 1, g f)
              where
                g _f i accum = if i <= 0 then accum else _f (i - 1) (i * accum)

            -- fibonacchi
            fibonacchiFix :: Integral a => (a -> a) -> a -> a
            fibonacchiFix fib n = if n <= 1 then n else fib (n - 1) + fib (n - 2)
            fibonacciLoop (n, f) = (f n, fibonacchiFix f)

            -- if f(n) = sort O() of length n
            --         = O(n * 2  f(n/2))
            --         = O((n * n/2 * n/4 ...) 2^log n)
            --         = O(n^2)
            -- quicksort
            quickSortFix :: (Ord a) => ([a] -> [a]) -> [a] -> [a]
            quickSortFix sort list
              | (x : xs) <- list =
                  let (less, gt) = partition (< x) xs
                   in sort less ++ (x : sort gt)
              | otherwise = []
            quickSortLoop (n, f) = (f n, quickSortFix f)

        let testLoop f testName = do
              printBanner $ "start:" ++ testName
              printList $ f <$> [1 .. 10]
              printBanner $ "done:" ++ testName

        testLoop (loop factorialLoop2) "loop+factorialLoop2"
        testLoop (loop2 factorialLoop2) "loop2+factorialLoop2"

        testLoop (fix factorialFix) "fix+factorialFix"
        testLoop (loop factorialLoop) "loop+factorialLoop"
        testLoop (loop2 factorialLoop) "loop2+factorialLoop"

        testLoop (fix fibonacchiFix) "fix+fibonacci0"
        testLoop (loop fibonacciLoop) "loop+fibonacci"
        testLoop (loop2 fibonacciLoop) "loop2+fibonacci"

        let testList = [100, 5, 20, 9, 53, 13, -33, 1, 3, -1, 2, 3, 2, 1]
        print $ fix quickSortFix testList
        print $ fix quickSortFix $ reverse testList
        print $ loop quickSortLoop testList
        print $ loop quickSortLoop $ reverse testList

        testDone
    )
    "testArrowLoop"
