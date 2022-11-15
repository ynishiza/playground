{-# LANGUAGE OverloadedStrings #-}

module TestUtils
  ( prependLabel,
    prependLabelStr,
    trace,
    traceShow,
    traceShowId,
    assert,
    assertIO,
    assertIsEqual,
    testDone,
    pauseIO,
    TestState,
    createTest,
    wrapTest,
    runTest,
    printBanner,
    printList,
    Message,
    Name,
  )
where

import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import Data.Foldable
import Data.Functor.Identity
import Data.List (intercalate)
import Debug.Trace (trace)
import Fmt

type Message = String

type Name = String

traceShow :: Show a1 => a1 -> a2 -> a2
traceShow x = trace (show x)

traceShowId :: Show a1 => String -> a1 -> a1
traceShowId label x = trace (label |+ " " +|| x ||+ "") x

prependLabel :: Show a => String -> a -> String
prependLabel label v = prependLabelStr label (show v)

prependLabelStr :: String -> String -> String
prependLabelStr label v = label ++ "=" ++ v

assert :: Bool -> ()
assert cond =
  if cond then () else error "Assertion failed"

assertIO :: Bool -> Message -> IO ()
assertIO cond message = do
  unless cond $ error $ "FAIL:" ++ message
  print message

assertIsEqual :: (Eq a, Show a) => a -> a -> IO ()
assertIsEqual x y = do
  unless (x == y) $ error $ fmtLn $ "assertIsEqual:" +|| x ||+ " != " +|| y ||+ ""
  fmtLn $ "assertIsEqual:" +|| x ||+ " == " +|| y ||+ ""

testDone :: IO ()
testDone = return ()

printBanner :: Name -> IO ()
printBanner name = putStrLn $ "=====" ++ name ++ "====="

pauseIO :: IO ()
pauseIO = do
  putStrLn "Press any key to continue"
  _ <- getChar
  putStrLn ""

type TestState = StateT [Message] (State (IO ())) ()

runTest :: TestState -> IO ()
runTest tests = do
  let inner = runStateT tests []
      (((), messages), io) = runIdentity $ runStateT inner (pure ())
   in do
        io
        putStrLn $ "tests:" ++ intercalate "," messages
        putStrLn $ "count=" ++ show (length messages)
        pauseIO

useIO :: IO () -> TestState
useIO next = lift $ modify (>> next)

createTest :: IO () -> Message -> TestState
createTest x message = do
  useIO
    ( do
        when (null message) (error "Missing name")
        printBanner $ "start:" ++ message
        x
        printBanner $ "end:" ++ message
    )
  modify (++ [message])

wrapTest :: TestState -> Message -> TestState
wrapTest tests message = do
  useIO $ printBanner $ "start:" ++ message
  tests
  useIO $ printBanner $ "end:" ++ message

printList :: (Foldable f, Show a) => f a -> IO ()
printList = traverse_ print
