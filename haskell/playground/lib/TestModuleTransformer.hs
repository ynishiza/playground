module TestModuleTransformer (
  -- testParser
  testMonadTransform,
  testStateTransformMonad,
  testLazyStateMonad,
  runAll,
) where

import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.State.Lazy
import TestUtils

runAll = callTest (do
  testMonadTransform
  testStateTransformMonad
  testLazyStateMonad
                  ) "TestModuleTransformer"

testMonadTransform =
  callTest
    ( do
        printBanner "MaybeT"
        let x = MaybeT [Just 1]
        let x0 :: MaybeT [] Int; x0 = lift [1]
        let y :: MaybeT (Either Char) Int; y = MaybeT $ Right (Just 1)
        let y0 :: MaybeT (Either Char) Int; y0 = lift $ Right 1
        print x
        print x0
        print y
        print y0
        -- print $ y == y0
        -- print $ runMaybeT y == runMaybeT y0
        assertIO (runMaybeT x == runMaybeT x0) "x==x0"
        assertIO (runMaybeT y == runMaybeT y0) "y==y0"
        print $ runMaybeT x
        print $ runMaybeT x0
        print $ runMaybeT y
        print $ runMaybeT y0
        testDone
    )
    "testTemplate"

testStateTransformMonad =
  callTest
    ( do
        let x = StateT (\s -> if even s then Right (True, s) else Left s)
        testDone
    )
    "testStateTransformMonad"

testLazyStateMonad =
  callTest
    ( do
        let task1 :: Num a => State a a
            task1 = do
              v <- get
              put (v * 10)
              get
        print $ runState task1 1
        print $ runState task1 2
        testDone
    )
    "testLazyStateMonad"

