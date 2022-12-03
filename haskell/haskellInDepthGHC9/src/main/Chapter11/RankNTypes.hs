{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}

module Chapter11.RankNTypes
  ( processIntTooNarrow,
    processInt,
    processIntTooWide,
    processIntUndefinable,
    processIntWithNumUnSafe,
    run,
    MyUtils(..),
  )
where

import Unsafe.Coerce
import Utils

run :: TestState
run =
  createChapterTest
    "11.5.2"
    "RankNTypes"
    ( do
        let listInt :: [Int]
            listInt = [1 .. 10]
            listDouble :: [Double]
            listDouble = [1.1,1.2,1.3]
         in do
              printBanner "Interface limiting"
              print $ processIntTooNarrow fNum listInt
              print $ processIntTooNarrow fInt listInt

              print $ processIntTooWide fNum listInt
              print $ processIntTooWide fNum listDouble

              print $ processInt fNum listInt
              -- print $ processInt fInt l     ERROR
              putStrLn "Done"
        testDone
    )

fNum :: Num a => a -> a
fNum = (* 10)

fInt :: Int -> Int
fInt = (* 100)

-- BAD: accepts only Int functions 
processIntTooNarrow :: (Int -> Int) -> [Int] -> [Int]
processIntTooNarrow f l = f <$> l

-- BAD: too generic
processIntTooWide :: Num a => (a -> a) -> [a] -> [a]
processIntTooWide f l = f <$> l

-- BAD: this is what we want, but undefinable as is
processIntUndefinable :: Num a => (a -> a) -> [Int] -> [Int]
-- processIntUndefinable f l = f <$> l      -- ERROR "Couldn't match type a with Int"
processIntUndefinable = undefined

-- GOOD: accepts only strictly generic Num functions
processInt :: (forall a. Num a => a -> a) -> [Int] -> [Int]
processInt f l = f <$> l

processIntWithNumUnSafe :: Num a => (a -> a) -> [Int] -> [Int]
processIntWithNumUnSafe f l = unsafeCoerce f <$> l

data MyUtils = MyUtils !(forall a. Show a => a -> IO ()) !(forall b. Show b => b)
data MyPrinter = MyPrinter !(forall a. Show a => a -> IO ()) 
-- data MyPrinter = MyPrinter (Show a => a -> IO ())
