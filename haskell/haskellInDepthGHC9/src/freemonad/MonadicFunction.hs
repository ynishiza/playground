{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}

module MonadicFunction (doA, doB, doC, doAF, doBF, doCF, Do (..), run) where

import Control.Monad
import Control.Monad.Free
import Control.Monad.IO.Class
import Data.Function

-- setup: original functions
doA :: MonadIO m => m String
doA = pure "A: Hello"

doB :: MonadIO m => m ()
doB = liftIO $ putStrLn "B: Hello"

doC :: MonadIO m => Int -> m Bool
doC = pure . even

doD :: (Show s, MonadIO m) => m s -> m String
doD = (show <$>)

-- setup: free data type for simulation
data Do s a where
  DoA :: (String -> a) -> Do s a
  DoB :: (() -> a) -> Do s a
  DoB' :: a -> Do s a
  DoC :: Int -> (Bool -> a) -> Do s a
  DoD :: s -> (String -> a) -> Do s a
  DoIO :: IO a -> Do s a
  deriving stock (Functor)

type DoF s = Free (Do s)

-- setup: simulating functions
doAF :: DoF s String
doAF = liftF $ DoA id

doBF :: DoF s ()
doBF = liftF $ DoB id

doB'F :: DoF s ()
doB'F = liftF $ DoB' ()

doCF :: Int -> DoF s Bool
doCF x = liftF $ DoC x id

doDF :: DoF s s -> DoF s String
doDF x = x >>= liftF . flip DoD id

doIO :: IO a -> DoF s a
doIO = liftF . DoIO

-- fold
foldDoF :: forall m s a. (Show s, MonadIO m) => DoF s a -> m a
foldDoF = iterM f
  where
    f :: Do s (m a) -> m a
    f (DoA k) = doA >>= k
    f (DoB k) = doB >> k ()
    f (DoB' a) = doB >> a
    f (DoC i k) = doC i >>= k
    f (DoD i k) = doD (pure i) >>= k
    f (DoIO io) = join (liftIO io)

run :: IO ()
run = do
  ( do
      x <- doAF
      doIO $ putStrLn x
      doBF
      doB'F
      doCF 1 >>= doIO . putStrLn . ("1 even:" <>) . show
      doCF 2 >>= doIO . putStrLn . ("2 even:" <>) . show
      doDF (pure (1 :: Int)) >>= doIO . putStrLn . ("pure 1:" <>)
    )
    & foldDoF

  putStrLn "Done"
