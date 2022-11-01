module Transformer.TestBase
  ( testMonadTransform,
    testLazyStateMonad,
    testTreeToNumber,
    allTests,
  )
where

import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.State.Lazy
import TestUtils
import Transformer.TestLabellingTrees
import Transformer.TestStateMonadExample
import Transformer.TestMyStateMonad

allTests :: TestState
allTests =
  wrapTest
    ( do
        testMonadTransform
        testLazyStateMonad
        testTreeToNumber
        Transformer.TestLabellingTrees.testTreeToNumber
        Transformer.TestMyStateMonad.testStateMonad
        Transformer.TestStateMonadExample.testStateMonad
    )
    "TestModuleTransformer"

testMonadTransform :: TestState
testMonadTransform =
  createTest
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
    "testMonadTransform"

testLazyStateMonad :: TestState
testLazyStateMonad =
  createTest
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
