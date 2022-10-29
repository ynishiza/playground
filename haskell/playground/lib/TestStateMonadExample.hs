{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}
module TestStateMonadExample (
  runTest
) where

import TestUtils

newtype State s a = State { runState :: s -> (a,s) }

instance Functor (State s) where
  fmap f (State run) = State (\s -> (f $ fst (run s), s))

instance Applicative (State s) where
  pure a = State $ \s -> (a, s)
  State f <*> State x = State (\s -> ((fst $ f s) (fst $ x s), s))

instance Monad (State s) where
  State act >>= k = State $ \s -> let (a, s') = act s
    in runState (k a) s'

get :: State s s
get = State $ \s -> (s, s)

put :: s -> State s ()
put s = State $ const ((), s)

putV :: a -> State s a
putV = pure

modify :: (s -> s) -> State s ()
modify f = get >>= \x -> put (f x)
evalState :: State s a -> s -> a
evalState act = fst . runState act
execState :: State s a -> s -> s
execState act = snd . runState act

runTest :: IO ()
runTest = callTest (do
  let
    test :: Num a => a -> State a a
    test _ = do
            put 1
            v <- get
            return $ v+1
            -- modify (+1)
            -- x <- get
            -- return (x * 3)

  print $ (\n -> runState (test n) 0) <$> [1..10]
  testDone
  ) "testMonadSampel"
