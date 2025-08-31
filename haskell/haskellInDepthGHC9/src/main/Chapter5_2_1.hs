{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use <&>" #-}

module Chapter5_2_1 (run) where

import Control.Monad.State
import Control.Monad
import System.Random
import Utils

run :: TestState
run =
  createChapterTest
    "5.2.1"
    "Random"
    ( do
        testChapter5_2_1

        print $ doFor (Just [1 .. 10]) (Just . (* 2))
        print $ doFor (return [1 .. 10]) (Just . (* 2))
        pure ()
    )

type Gen = StdGen

type RandomWork a = State Gen a

testChapter5_2_1 :: IO ()
testChapter5_2_1 =
  do
    printBanner "Chapter 5.2.1"

    let task :: RandomWork [Int]
        task = do
          v <- getRandomMany (getRandom @Int) 100
          vR <- getRandomMany ((getRandomR @Int) (1, 1000)) 100
          return $ v ++ vR
     in runRandom task >>= print

runRandom :: RandomWork a -> IO a
runRandom r = getStdGen >>= return . evalState r

getRandomMany :: RandomWork a -> Int -> RandomWork [a]
getRandomMany r n = replicateM n r

getRandom :: Uniform a => RandomWork a
getRandom = useRandom uniform

getRandomR :: UniformRange a => (a, a) -> RandomWork a
getRandomR r = useRandom (uniformR r)

useRandom :: (Gen -> (a, Gen)) -> RandomWork a
useRandom f = do
  (v, g) <- gets f
  put g
  return v

doFor :: (Monad m, Traversable f) => m (f a) -> (a -> m b) -> m (f b)
doFor m f = m >>= traverse f
