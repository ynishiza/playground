{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use uncurry" #-}
module Transformer.TestBase
  ( testMonadTransform,
    testLazyStateMonad,
    testTreeToNumber,
    testStateWithAndWithoutMonads,
    testReadWriteState,
    allTests,
  )
where

import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State.Lazy
import Control.Monad.Trans.Writer.Lazy
import TestUtils
import Transformer.TestLabellingTrees
import Transformer.TestMyStateMonad
import Transformer.TestStateMonadExample

allTests :: TestState
allTests =
  wrapTest
    ( do
        testMonadTransform
        testLazyStateMonad
        testTreeToNumber
        testReadWriteState
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

data User = User {name :: String, count :: Int} deriving (Show, Eq)

type ComputeStateData = ([Int], IO ())

testStateWithAndWithoutMonads :: TestState
testStateWithAndWithoutMonads =
  createTest
    ( do
        -- state without monad
        let example :: IO ()
            example = do
              let compute :: (Int, ([Int], IO ())) -> (Int, ([Int], IO ()))
                  compute (x, (items, console)) =
                    let items' = items ++ [x]
                        io' = do console; putStrLn $ "x=" ++ show x
                     in (x * 2, (items', io'))

                  (y1, state1) = compute (1, ([], pure ()))
                  (y2, state2) = compute (y1, state1)
                  (y3, state3) = compute (y2, state2)
                  (items3, console3) = state3
               in do
                    console3
                    putStrLn $ "items=" ++ show items3 ++ "y=" ++ show y3
         in example

        -- same example with State Monads
        let example :: IO ()
            example = do
              let compute :: Int -> State ([Int], IO ()) Int
                  compute x = do
                    (items, console) <- get
                    let items' = items ++ [x]
                        console' = do console; putStrLn $ "x=" ++ show x
                    put (items', console')
                    return (x * 2)

                  task1 :: State ComputeStateData Int
                  task1 = do
                    y1 <- compute 1
                    y2 <- compute y1
                    compute y2
                  (y3, (items3, console3)) = runState task1 ([], pure ())

                  task2 = do
                    _ <- task1
                    _ <- task1
                    task1
                  (y4, (items4, console4)) = runState task2 ([], pure ())
               in do
                    console3
                    putStrLn $ "items=" ++ show items3 ++ "y=" ++ show y3

                    console4
                    putStrLn $ "items=" ++ show items4 ++ "y=" ++ show y4
         in example

        testDone
    )
    "testStateWithAndWithoutMonads"

testReadWriteState :: TestState
testReadWriteState =
  createTest
    ( do
        let writeTask :: Writer String Int
            writeTask = do
              tell "hello"
              tell "world"
              return 1

            readTask :: Reader [Int] Int
            readTask = do
              list <- ask
              v1 <- asks (!! 0)
              v2 <- asks (!! 1)
              return $ v1 + v2 + last list

            readTask2 = local ((* 2) <$>) readTask

            printWriter :: (Show w, Show a) => Writer w a -> IO ()
            printWriter w =
              let (_state, result) = runWriter w
               in putStrLn $ prependLabel "state" _state ++ prependLabel " result" result

            printReader :: Show a => Reader r a -> r -> IO ()
            printReader r readOnlyState =
              let result = runReader r readOnlyState
               in putStrLn $ prependLabel "result" result

        printWriter writeTask

        printReader readTask [1 .. 10]
        printReader readTask2 [1 .. 10]

        testDone
    )
    "testReadWriteState"
