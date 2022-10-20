module TestUtils
  ( assert,
    assertIO,
    testDone,
    callTest,
    printBanner,
    printList,
  )
where

import Control.Monad
import Data.Foldable

assert :: Bool -> ()
assert cond =
  if cond then () else error "Assertion failed"

assertIO :: Bool -> String -> IO ()
assertIO cond message = do
  print message
  unless cond $ error $ "FAIL:" ++ message

testDone :: IO ()
testDone = return ()

printBanner :: String -> IO ()
printBanner msg = putStrLn $ "=====" ++ msg ++ "====="

callTest :: IO () -> String -> IO ()
callTest x message = do
  printBanner $ "start:" ++ message
  x
  printBanner $ "end:" ++ message

printList :: (Foldable f, Show a) => f a -> IO ()
printList = traverse_ print
