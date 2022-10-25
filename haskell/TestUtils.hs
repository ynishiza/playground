module TestUtils
  ( assert,
    assertIO,
    testDone,
    callTest,
    printBanner,
    printList,
    Message,
    Name,
  )
where

import Control.Monad
import Data.Foldable

type Message = String
type Name = String

assert :: Bool -> ()
assert cond =
  if cond then () else error "Assertion failed"

assertIO :: Bool -> Message -> IO ()
assertIO cond message = do
  print message
  unless cond $ error $ "FAIL:" ++ message

testDone :: IO ()
testDone = return ()

printBanner :: Name -> IO ()
printBanner name = putStrLn $ "=====" ++ name ++ "====="

callTest :: IO () -> Message -> IO ()
callTest x message = do
  when (null message) (error "Missing name")
  printBanner $ "start:" ++ message
  x
  printBanner $ "end:" ++ message

printList :: (Foldable f, Show a) => f a -> IO ()
printList = traverse_ print
