{-# LANGUAGE OverloadedStrings #-}
{-# HLINT ignore "Use camelCase" #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Chapter5_3_1 (run) where

import Control.Monad.Extra (ifM)
import Data.Foldable
import Data.IORef
import Data.Monoid
import Data.STRef
import Fmt
import GHC.ST
import System.Directory.Extra (doesDirectoryExist, listContents)
import Utils

run :: TestState
run =
  wrapTest
    ( do
        testIORefSimple
        testSTSimple
        testFiles
        useIO $ print $ doFor (Just [1 .. 10]) (Just . (* 2))
        useIO $ print $ doFor (return [1 .. 10]) (Just . (* 2))
    )
    "Chapter 5.3.1"

doFor :: (Monad m, Traversable f) => m (f a) -> (a -> m b) -> m (f b)
doFor m f = m >>= traverse f

testIORefSimple :: TestState
testIORefSimple =
  createChapterTest
    "5.3.1"
    "IORef"
    ( do
        ref <- newIORef (1 :: Int)
        let readValue = do
              v <- readIORef ref
              fmtLn $ "current value:" +| v |+ ""
        readIORef ref >>= print
        traverse_ (\x -> writeIORef ref x >> readValue) [1 .. 10]
        traverse_ (\x -> modifyIORef ref (+ x) >> readValue) [1 .. 10]
        testDone
    )

testSTSimple :: TestState
testSTSimple =
  createChapterTest
    "5.3.1"
    "ST"
    ( do
        let factorial :: Int -> Int
            factorial n = runST $ do
              ref <- newSTRef 1
              let f m
                    | m <= 0 = pure ()
                    | otherwise = modifySTRef ref (* m) >> f (m -1)
               in f n
              readSTRef ref

            factorial' n = getProduct $ fold $ Product <$> [1 .. n]

        traverse_ (assertIsEqual <$> factorial <*> factorial') [1 .. 10]
        testDone
    )

testFiles :: TestState
testFiles =
  createChapterTest
    "1.5.3"
    "IORef + file"
    ( do
        listAllContent "src" >>= fmtLn . blockListF
        listAllContent "lib" >>= fmtLn . blockListF
        listAllContent "test" >>= fmtLn . blockListF
        srcFiles <- listAllContent "src"
        libFiles <- listAllContent "lib"
        assertIsEqual True ("src/main" `elem` srcFiles)
        assertIsEqual True ("lib/Utils.hs" `elem` libFiles)
    )

listAllContent :: FilePath -> IO [FilePath]
listAllContent path = do
  list <- newIORef []
  let visit fp = do
        modifyIORef list (fp :)

        -- recursively visit subdirectories
        ifM
          (doesDirectoryExist fp)
          (listContents fp >>= traverse_ visit)
          (pure ())

  visit path
  readIORef list
