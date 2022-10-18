module TestMonad
  ( testMonad,
    testMonadFix,
    testWrappedMonad,
    testCompositionMonad,
    testFunctorMonad,
    testMonadFail,
    testMonadTransform,
    testStateMonad,
    testStaticArrow,
  )
where

import Control.Applicative
import Control.Arrow
import Control.Monad
import Control.Monad.Fix
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.State.Lazy
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

testMonadTransform =
  callTest
    ( do
        printBanner "MaybeT"
        let x = MaybeT [Just 1]
        let x0 :: MaybeT [] Int; x0 = lift [1]
        let y :: MaybeT (Either Char) Int; y = MaybeT $ Right (Just 1)
        let y0 :: MaybeT (Either Char) Int; y0 = lift $ Right 1
        print x
        print x0
        print y
        print y0
        -- print $ y == y0
        -- print $ runMaybeT y == runMaybeT y0
        assertIO (runMaybeT x == runMaybeT x0) "x==x0"
        assertIO (runMaybeT y == runMaybeT y0) "y==y0"
        print $ runMaybeT x
        print $ runMaybeT x0
        print $ runMaybeT y
        print $ runMaybeT y0
        testDone
    )
    "testTemplate"

testStateMonad =
  callTest
    ( do
        let x = StateT (\s -> if even s then Right (True, s) else Left s)
        testDone
    )
    "testStateMonad"

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
        -- let f = mfix (\fact -> (\n -> if n <= 1 then n else n * fact (n-1)))
        printBanner "Factorial"
        let execFactorial fact n = if n <= 1 then n else n * fact (n - 1)
        -- let f = mfix (\fact -> (\n -> if n <= 1 then n else n * fact (n-1)))
        let mfixFact1 = mfix (\fact _ -> execFactorial fact)
        let mfixFact2 = return (fix execFactorial) :: Integer -> Integer -> Integer
        let mfixFact3 = mfix (return . execFactorial) :: Integer -> Integer -> Integer
        printList $ mfixFact1 0 <$> [1 .. 10]
        printList $ mfixFact1 1 <$> [1 .. 10]
        printList $ mfixFact2 0 <$> [1 .. 10]
        printList $ mfixFact3 0 <$> [1 .. 10]

        printBanner "Factorial - list"
        let execHalf f n = if odd n || n <= 1 then [] else n : f (div n 2)
        let mfixHalf1 = mfix (\f _ -> execHalf f)
        let mfixHalf2 = return (fix execHalf) :: (Integral a, Ord a) => [a -> [a]]
        let mfixHalf3 = mfix (return . execHalf) :: (Integral a, Ord a) => [a -> [a]]
        printList $ mfixHalf1 [1 .. 20] 0
        printList $ mfixHalf2 <*> [1 .. 20]
        printList $ mfixHalf3 <*> [1 .. 20]

        -- print $ f 2
        let x = 1
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
