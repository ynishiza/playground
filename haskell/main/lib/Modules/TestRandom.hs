{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Modules.TestRandom (runAll, testStatefulGen) where

import Control.Monad
import Data.Word
import Data.STRef
import GHC.ST
import Fmt
import System.Random
import System.Random.Stateful
import TestUtils

runAll =
  wrapTest
    ( do
        testPure
        testStatefulGen
    )
    "TestRandom"

testPure :: TestState
testPure =
  createTest
    ( do
        do
          let g0 = mkStdGen 0
              (v1, g1) = uniform @StdGen @Int g0
              (v2, _) = uniform @StdGen @Int g1
          print (v1, v2)

          let g0 = mkStdGen 0
              g0' = mkStdGen 0
              (v1, g1) = uniform @StdGen @Int g0
              (v1', g1') = uniform @StdGen @Int g0
              (v1'', g1'') = uniform @StdGen @Int g0'
          print (v1, v1', v1'')
          print $ g1 == g1'
          print $ g1 == g1''

        do
          initStdGen >>= setStdGen
          do
            g0 <- initStdGen
            let (v, g0') = uniform @StdGen @Int g0
                (v', g0'') = uniform @StdGen @Int g0'
            setStdGen g0''
            print (v,v')

          do
            g0 <- getStdGen
            let (v, g0') = uniform @StdGen @Int g0
            setStdGen g0'
            print v

          newStdGen >>= setStdGen
          v1 <- getStdRandom @IO @Int uniform
          v2 <- getStdRandom @IO @Int uniform
          print $ v1 == v2
          print (v1, v2)
        testDone

        printBanner "STGenM"
        print useSTRandom
        print useSTRandom
    )
    "testPure"

useSTRandom :: (Int, Int)
useSTRandom = runST $ do
  g0 <- newSTGenM (mkStdGen 0)
  v1 <- uniformM @Int g0
  v2 <- uniformM @Int g0
  return (v1,v2)

manyRandomPure :: Int -> IO [Int]
manyRandomPure n = replicateM n (getStdRandom (uniform @StdGen @Int))

manyRandomStateful :: Int -> IO [Int]
manyRandomStateful n = replicateM n (getStdGen >>= newIOGenM >>= uniformM @Int)

testStatefulGen :: TestState
testStatefulGen =
  createTest
    ( do
        printBanner "Use IOGen"
        do
          iog <- newStdGen >>= newIOGenM
          v1 <- uniformM @Int iog 
          v2 <- uniformM @Int iog 
          print (v1, v2)
          getRandomPairStateful @Word8 iog >>= fmtLn . tupleF
          getRandomNStateful @Word8 iog 5 >>= fmtLn . listF

        printBanner "Use AtomicGen"
        do
          atomicG <- newStdGen >>= newAtomicGenM
          v1 <- uniformM @Int atomicG 
          v2 <- uniformM @Int atomicG
          print (v1, v2)
          getRandomPairStateful @Word8 atomicG >>= fmtLn . tupleF
          getRandomNStateful @Word8 atomicG 5 >>= fmtLn . listF

        printBanner "Replication"
        manyRandomPure 5 >>= print
        manyRandomStateful 5 >>= print

        
        printBanner "Custom method"
        do
          g0 <- initStdGen
          p0 <- getStdRandom (uniform @StdGen @(MyPair Int))
          p1 <- getStdRandom (uniform @StdGen @(MyPair Int))
          print (p0, p1)

        do
          iog <- initStdGen >>= newIOGenM 
          p0 <- uniformM @(MyPair Int) iog
          p1 <- uniformM @(MyPair Int) iog
          print (p0, p1)
        testDone
    )
    "testStatefulGen"

getRandomPairBad :: Uniform a => IO (a, a)
getRandomPairBad = do
  g <- getStdGen
  let v1 = fst $ uniform g
      v2 = fst $ uniform g
  return (v1, v2)

getRandomPairStateful :: (Uniform a, StatefulGen g m) => g -> m (a, a)
getRandomPairStateful g = do
  v1 <- uniformM g
  v2 <- uniformM g
  return (v1, v2)

getRandomNStateful :: (Uniform a, StatefulGen g m) => g -> Int -> m [a]
getRandomNStateful g n = replicateM n (uniformM g)

data MyPair a = MyPair a a deriving (Show, Eq)

instance Uniform a => Uniform (MyPair a) where
  uniformM g = do
    v1 <- uniformM @a g
    v2 <- uniformM @a g
    return (MyPair v1 v2)


