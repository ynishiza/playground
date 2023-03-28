{-# LANGUAGE GADTs #-}

module MonadicFunction (doA, doB, doC, doAF, doBF, doCF, Do (..), run) where

import Control.Monad.Free
import Control.Monad.IO.Class
import Data.Function
import Data.Functor.Classes

-- setup: original functions
doA :: MonadIO m => m String
doA = pure "A: Hello"

doB :: MonadIO m => m ()
doB = liftIO $ putStrLn "B: Hello"

doC :: MonadIO m => Int -> m Bool
doC = pure . even

doD :: (Show s, MonadIO m) => m s -> m String
doD = (show <$>)

doE :: Monad m => m a -> (a -> b) -> m b
doE = flip fmap

doIO :: MonadIO m => IO a -> m a
doIO = liftIO

-- setup: free data type for simulation
data Do a where
  DoA :: (String -> a) -> Do a
  DoB :: (() -> a) -> Do a
  DoB' :: a -> Do a
  DoC :: Int -> (Bool -> a) -> Do a
  DoD :: Show s => s -> (String -> a) -> Do a
  DoE :: x -> (x -> y) -> (y -> a) -> Do a
  DoIO :: IO a -> Do a

instance Functor Do where
  fmap f (DoA k) = DoA $ f . k
  fmap f (DoB k) = DoB $ f . k
  fmap f (DoB' a) = DoB' $ f a
  fmap f (DoC i k) = DoC i $ f . k
  fmap f (DoD s k) = DoD s $ f . k
  fmap f (DoE x g k) = DoE x g $ f . k
  fmap f (DoIO io) = DoIO $ f <$> io

instance Show1 Do where
  liftShowsPrec showA _ i (DoA f) = ("DoA" <>) . showA i (f "")
  liftShowsPrec showA _ i (DoB f) = ("DoB" <>) . showA i (f ())
  liftShowsPrec showA _ i (DoB' f) = ("DoB'" <>) . showA i f
  liftShowsPrec showA _ i (DoC _ f) = ("DoC" <>) . showA i (f True)
  liftShowsPrec showA _ i (DoD _ k) = ("DoD" <>) . showA i (k "")
  liftShowsPrec showA _ i (DoE x f k) = ("DoE" <>) . showA i (k $ f x)
  liftShowsPrec _ _ _ (DoIO _) = ("DoIO" <>)

-- setup: Free
type DoF = Free Do

-- setup: simulating functions
doAF :: DoF String
doAF = liftF $ DoA id

doBF :: DoF ()
doBF = liftF $ DoB id

doB'F :: DoF ()
doB'F = liftF $ DoB' ()

doCF :: Int -> DoF Bool
doCF x = liftF $ DoC x id

doDF :: Show s => DoF s -> DoF String
doDF x = x >>= liftF . flip DoD id

doEF :: DoF x -> (x -> y) -> DoF y
doEF dx f = dx >>= \x -> liftF (DoE x f id)

doIOF :: IO a -> DoF a
doIOF = liftF . DoIO

-- fold
foldDoF :: forall m a. (MonadIO m) => DoF a -> m a
foldDoF = foldFree f
  where
    f :: Do x -> m x
    f (DoA k) = k <$> doA
    f (DoB k) = k <$> doB
    f (DoB' a) = doB >> return a
    f (DoC i k) = k <$> doC i
    f (DoD s k) = k <$> doD (pure s)
    f (DoE x g k) = k <$> doE (pure x) g
    f (DoIO io) = doIO io

run :: IO ()
run = do
  let c = do
        x <- doAF
        doIOF $ putStrLn x
        doBF
        doB'F
        doCF 1 >>= doIOF . putStrLn . ("1 even:" <>) . show
        doCF 2 >>= doIOF . putStrLn . ("2 even:" <>) . show
        doEF (pure (2 :: Int)) (* (2 :: Int)) >>= doIOF . putStrLn . ("map:" <>) . show
        doDF (pure (1 :: Int)) >>= doIOF . putStrLn . ("pure 1:" <>)

  foldDoF c
  ( do
      x <- doAF
      doBF
      doB'F
      _ <- doCF 1
      _ <- doDF (pure x)
      _ <- doEF (pure (1 :: Int)) (* 2)
      pure ()
    )
    & show
    & print

  putStrLn "Done"
