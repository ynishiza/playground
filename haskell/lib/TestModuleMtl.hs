{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use tuple-section" #-}
module TestModuleMtl
  ( testMyIOState,
    testBinarySequenceState,
    testComposeState,
    runAll,
  )
where

import Control.Monad.State
import Data.Foldable (traverse_)
import TestUtils

runAll :: IO ()
runAll =
  callTest
    (do testMyIOState; testBinarySequenceState; testComposeState)
    "TestModuleMtl"

newtype MyIOState a = MyIOState (IO a)

getMyIOState :: MyIOState a -> IO a
getMyIOState (MyIOState x) = x

instance Functor MyIOState where
  fmap f (MyIOState io) = MyIOState (fmap f io)

instance Applicative MyIOState where
  pure = MyIOState . pure
  (MyIOState f) <*> (MyIOState x) = MyIOState (f <*> x)

instance Monad MyIOState where
  (MyIOState io) >>= k = MyIOState (io >>= (getMyIOState . k))

instance MonadState String MyIOState where
  -- state f = MyIOState (do
  --   x <- getLine
  --   let (a, s) = f x
  --   putStrLn s
  --   return a
  -- )
  get = MyIOState getLine
  put = MyIOState . putStrLn

testMyIOState :: IO ()
testMyIOState =
  callTest
    ( do
        let query :: MyIOState ()
            query = do
              put "Enter value"
              s <- get
              put $ "Current value:" ++ s

        getMyIOState
          ( do
              put "Hello"
              query
              query
              query
          )

        testDone
    )
    "testMyIOState"

type StateWithIOData a = (IO (), a)

type StateWithIO m a = StateT (IO (), a) m ()

type BinarySequenceState = StateWithIO [] [Int]

putMap :: Monad m => IO () -> (a -> a) -> StateWithIO m a
putMap next f = StateT (\(io, v) -> pure ((), (do io; next, f v)))

useIO :: Monad m => IO () -> StateWithIO m a
useIO next = putMap next id

setValueOnly :: Monad m => a -> StateWithIO m a
setValueOnly v = putMap (pure ()) (const v)

execStateWithIO :: Monad m => StateWithIO m a -> a -> m (IO (), a)
execStateWithIO st v = execStateT st (pure (), v)

printStateWithIOResult :: (Foldable t, Show a) => t (StateWithIOData a) -> IO ()
printStateWithIOResult =
  traverse_
    ( \(io, v) -> do
        putStrLn $ "result=" ++ show v
        putStr "  io="
        io
        putStrLn ""
    )

execAndPrintStateWithIO :: (Show a, Foldable m, Monad m) => StateWithIO m a -> a -> IO ()
execAndPrintStateWithIO st = printStateWithIOResult . execStateWithIO st

prependLabel :: Show a => String -> a -> String
prependLabel label x = label ++ "=" ++ show x

testBinarySequenceState :: IO ()
testBinarySequenceState =
  callTest
    ( do
        let appendBit :: BinarySequenceState
            appendBit =
              StateT
                ( \(io, v) ->
                    let printNext _v = do
                          io
                          putStrLn $ prependLabel "current" v ++ prependLabel "  next" _v
                        append bit = ((), (printNext $ v ++ [bit], v ++ [bit]))
                     in [append 0, append 1]
                )

            sequence1 = do
              useIO $ putStrLn "Session start"
              appendBit
              appendBit
              appendBit
              appendBit
              useIO $ putStrLn "Session done"
            sequence2 = do
              sequence1
              useIO $ putStrLn "Session start"
              appendBit
              appendBit
              appendBit
              appendBit
              useIO $ putStrLn "Session done"

            printB :: [(IO (), [Int])] -> IO ()
            printB re = do
              printStateWithIOResult re
              putStrLn $ prependLabel "count" (length re)
              _ <- getLine
              putStrLn ""

            result1 = execStateWithIO sequence1 []
            result12 = execStateWithIO sequence2 []

        _ <- getLine
        execAndPrintStateWithIO sequence1 [0, 1]
        printB result1
        printB result12

        testDone
    )
    "testBinarySequenceState"

type ComposeState a = StateWithIO ((->) (StateWithIOData a)) Int

testComposeState :: IO ()
testComposeState =
  callTest
    ( do
        let composeOne :: String -> (Int -> Int) -> ComposeState a
            composeOne label f =
              StateT
                ( \(io, x) ->
                    let y = f x
                        io' = do
                          io
                          putStrLn $ label ++ prependLabel " x=" x ++ prependLabel " y=" y
                     in const ((), (io', y))
                )

            addNState n = composeOne ('+' : show n) (+ n)
            multNState n = composeOne ('*' : show n) (* n)

            computeAdd :: ComposeState ()
            computeAdd = do
              addNState 1
              addNState 2
              addNState 3
              addNState 4
              addNState 5

            computeMult = do
              multNState 1
              multNState 2
              multNState 3
              multNState 4
              multNState 5

            result = execStateWithIO computeAdd 1 (pure (), ())
            result2 = execStateWithIO (computeAdd >> computeMult) 1 (pure (), ())

            printResult :: Show a => StateWithIOData a -> IO ()
            printResult (io, v) = do
              io
              putStrLn $ prependLabel "result" v
              _ <- getLine
              putStrLn ""

        printResult result
        assertIsEqual (snd result) 16

        printResult result2
        assertIsEqual (snd result2) 1920
        testDone
    )
    "testComposeState"
