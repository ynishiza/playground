{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use tuple-section" #-}

module Transformer.TestMyStateMonad
  ( -- State(..),
    --
    testStateMonad,
  )
where

import Control.Arrow
import TestUtils

-- StateOp: represents an operation on the state.
--
-- s = current state
-- o = result of the operation
--
-- The state cannot be accessed directly, but only through an operation,
-- including reading the current state.
-- e.g.
--    s -> (s,s)          read current state i.e. getOp
--    s -> (s+1,())       add 1 to the current state
--    s -> (10,())        reset the current state
--
--
--  Analogous to IO type
--  e.g.
--    -- IO
--    do
--      message <- getLine                  IO String
--      putStrLn $ "Hello " ++ message      IO ()
--
--    -- state
--    do
--      state <- getOp                      State Int Int
--      putOp (state * 3)                   State Int ()
--
nullResult :: ()
nullResult = ()

newtype StateOp s r = StateOp (s -> (s, r))

execOp :: StateOp s r -> s -> (s, r)
execOp (StateOp op) = op

-- write state operation
putOp :: s -> StateOp s ()
putOp state = StateOp $ const (state, nullResult)

-- get state operation
getOp :: StateOp s s
getOp = StateOp (\state -> (state, state))

-- map state operation
mapOp :: (s -> s) -> StateOp s ()
mapOp f = StateOp (\state -> (f state, nullResult))

multOp :: Num s => s -> StateOp s ()
multOp n = mapOp (* n)

addOp :: Num s => s -> StateOp s ()
addOp n = mapOp (+ n)

instance Functor (StateOp s) where
  fmap f (StateOp op) = StateOp $ op >>> second f

instance Applicative (StateOp s) where
  pure x = StateOp (\s -> (s, x))
  StateOp getMap <*> StateOp getResult =
    StateOp
      ( \state ->
          let (_, resultMap) = getMap state
              (_, result) = getResult state
           in (state, resultMap result)
      )

instance Monad (StateOp s) where
  (StateOp _execOp) >>= createOp =
    StateOp
      ( \state ->
          let (nextState, result) = _execOp state
              (StateOp nextOp) = createOp result
           in nextOp nextState
      )

main :: IO ()
main = do
  let initState = (-1)
      toString = do
        state <- getOp
        return $ "current state=" ++ show state

  -- 3(2x + 10)
  print $ execOp (multOp 2) initState
  print $ execOp (do multOp 2; addOp 10) initState
  print $ execOp (do multOp 2; addOp 10; multOp 3) initState
  print $ execOp (do multOp 2; addOp 10; multOp 3; toString) initState

testStateMonad :: TestState
testStateMonad =
  createTest
    main
    "testStateMonad"
