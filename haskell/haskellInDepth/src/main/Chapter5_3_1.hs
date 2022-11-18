{-# LANGUAGE OverloadedStrings #-}
{-# HLINT ignore "Use camelCase" #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Chapter5_3_1 (run) where

import Data.Foldable
import Data.IORef
import Fmt
import Utils

run :: TestState
run =
  createChapterTest
    "5.3.1"
    "IORef"
    ( do
        testIORefSimple

        print $ doFor (Just [1 .. 10]) (Just . (* 2))
        print $ doFor (return [1 .. 10]) (Just . (* 2))
        pure ()
    )

doFor :: (Monad m, Traversable f) => m (f a) -> (a -> m b) -> m (f b)
doFor m f = m >>= traverse f

testIORefSimple :: IO ()
testIORefSimple =
  do
    ref <- newIORef (1 :: Int)
    let readValue = do
          v <- readIORef ref
          fmtLn $ "current value:" +| v |+ ""
    readIORef ref >>= print
    traverse_ (\x -> writeIORef ref x >> readValue) [1 .. 10]
    traverse_ (\x -> modifyIORef ref (+ x) >> readValue) [1 .. 10]
    testDone
