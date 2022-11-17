{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}

module Chapter5 (run, runGcd, testShuntingYard) where

import Chapter5ParserTest
import Control.Monad.Trans
import Control.Monad.State
import Control.Monad.Writer
import Data.Foldable
import qualified Data.Text as T
import System.Random
import Fmt
import Utils

run :: IO ()
run = do
  testShuntingYard
  testRandom
  runGcd

  print $ doFor (Just [1..10]) (Just . (*2))
  print $ doFor (return [1..10]) (Just . (*2))
  pure ()

runGcd :: IO ()
runGcd = do
  printBanner "GCD with IO"
  res <- traverse (gcd_with_io (10 :: Integer)) [1 .. 10]
  print res

  printBanner "GCD with Writer"
  let f :: Int -> Int -> IO Int
      f x y = do
        prettyLn t
        return r
        where
          (r, t) = runWriter (gcd_with_textlog x y)
   in traverse_ (f 10) [1 .. 10]

  printBanner "GCD with nested writer"
  let f :: Int -> Int -> IO Int
      f x y = do
        prettyLn t
        return r
        where
          ((r, t), _) = runWriter $ runWriterT (gcd_with_textlog_depth x y)
   in traverse_ (f 10) [1 .. 10]
  pure ()

gcd_with_effect :: (Monad m, Integral a) => (a -> a -> m ()) -> a -> a -> m a
gcd_with_effect ef x 0 = ef x 0 >> return x
gcd_with_effect ef x y = ef x y >> gcd_with_effect ef y (x `mod` y)

gcd_with_io :: (Buildable a, Integral a) => a -> a -> IO a
gcd_with_io x y = do
  r <- gcd_with_effect ef x y
  fmt $ log_gcd_result x y r
  return r
  where
    ef x' y' = fmt $ log_gcd_step x' y'

gcd_with_textlog :: (Buildable a, Integral a) => a -> a -> Writer T.Text a
gcd_with_textlog x y = do
  r <- gcd_with_effect ef x y
  tell $ fmt $ log_gcd_result x y r
  return r
  where
    ef x' y' =
      tell $ fmt $ log_gcd_step x' y'

gcd_with_textlog_depth :: forall a. (Buildable a, Integral a) => a -> a -> WriterT T.Text (Writer (Sum Int)) a
gcd_with_textlog_depth x y = do
  let res = gcd_with_effect ef x y
      w = runWriterT res
  ((r, logs), c) <- lift $ listen w
  tell $
    logs |+ ""
      +| log_gcd_result x y r |+ ""
      +| nameF "depth" (build $ getSum c) |+ ""
  return r
  where
    ef :: a -> a -> WriterT T.Text (Writer (Sum Int)) ()
    ef x' y' = do
      tell $ fmt $ log_gcd_step x' y'
      lift $ tell $ Sum 1

log_gcd_result :: Buildable a => a -> a -> a -> Builder
log_gcd_result x y r = "gcd(" +| x |+ "," +| y |+ ") = " +| r |+ "\n"

log_gcd_step :: Buildable a => a -> a -> Builder
log_gcd_step x y = "x=" +| x |+ " y=" +| y |+ "\n"


type Gen = StdGen
type RandomWork a = State Gen a

testRandom :: IO ()
testRandom = do
  printBanner "random test"

  let 
    task :: RandomWork [Int]
    task = do
      v <- getRandomMany (getRandom @Int) 100
      vR <- getRandomMany ((getRandomR @Int) (1,1000)) 100
      return $ v ++ vR

     in runRandom task >>= print

runRandom :: RandomWork a -> IO a
runRandom r = getStdGen >>= return.evalState r

getRandomMany :: RandomWork a -> Int -> RandomWork [a]
getRandomMany r n = replicateM  n r

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

