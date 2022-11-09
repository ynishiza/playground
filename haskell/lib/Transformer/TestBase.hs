{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use uncurry" #-}
module Transformer.TestBase
  ( testMonadTransform,
    testTreeToNumber,
    -- testStateWithAndWithoutMonads,
    -- testReadWriteState,
    -- testNestedState,
    allTests,
  )
where

import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
-- import Control.Monad.Trans.Reader
-- import Control.Monad.Trans.State.Lazy
-- import Control.Monad.Trans.Writer.Lazy
import TestUtils
import Transformer.TestState
import Transformer.TestLabellingTrees
import Transformer.TestMyStateMonad
import Transformer.TestStateMonadExample

allTests :: TestState
allTests =
  wrapTest
    ( do
        testMonadTransform
        testTreeToNumber
        Transformer.TestState.testStateWithAndWithoutMonads
        Transformer.TestState.testReadState
        Transformer.TestState.testWriteState
        Transformer.TestState.testNestedState
        Transformer.TestState.testCont
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
