module TestUtils
  ( 
  prependLabel,
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
import Control.Monad.Trans.State
import Data.Foldable
import Data.List (intercalate)

type Message = String

type Name = String

prependLabel :: Show a => String -> a -> String
prependLabel label v = label ++ "=" ++ show v

assert :: Bool -> ()
assert cond =
  if cond then () else error "Assertion failed"

assertIO :: Bool -> Message -> IO ()
assertIO cond message = do
  unless cond $ error $ "FAIL:" ++ message
  print message

assertIsEqual :: (Eq a, Show a) => a -> a -> IO ()
assertIsEqual x y = do
  unless (x == y) $ error $ "assertIsEqual:" ++ show x ++ "!=" ++ show y
  putStrLn $ "assertIsEqual:" ++ show x ++ "==" ++ show y

testDone :: IO ()
testDone = return ()

printBanner :: Name -> IO ()
printBanner name = putStrLn $ "=====" ++ name ++ "====="

pauseIO :: IO ()
pauseIO = do
  putStrLn "Press any key to continue"
  _ <- getChar
  putStrLn ""

type TestState = State (IO (), [String]) ()

runTest :: TestState -> IO ()
runTest tests = do
  let (io, messages) = execState tests (pure (), [])
   in do
        io
        putStrLn $ "tests:" ++ intercalate "," messages
        putStrLn $ "count=" ++ show (length messages)
        pauseIO

useIO :: IO () -> TestState
useIO next = do
  (io, v) <- get
  put (do io; next, v)

createTest :: IO () -> Message -> TestState
createTest x message = do
  (io, messages) <- get
  let test = do
        io
        when (null message) (error "Missing name")
        printBanner $ "start:" ++ message
        x
        printBanner $ "end:" ++ message
   in put (test, messages ++ [message])

wrapTest :: TestState -> Message -> TestState
wrapTest tests message = do
  useIO $ printBanner $ "start:" ++ message
  tests
  useIO $ printBanner $ "end:" ++ message

printList :: (Foldable f, Show a) => f a -> IO ()
printList = traverse_ print
