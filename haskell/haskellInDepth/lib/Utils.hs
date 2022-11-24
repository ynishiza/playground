{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda using `infix`" #-}

module Utils
  ( (.@),
    (..@),
    pauseIO,
    whileLoop,
    promptRun,
    trace,
    traceShow,
    traceShowId,
    assertIsEqual,
    assertIsEqualSilent,
    printBanner,
    printBannerWrap,
    useIO,
    createTest,
    createChapterTest,
    wrapTest,
    runTest,
    TestState,
    testDone,
  )
where

import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import Data.Functor.Identity
import Data.List (intercalate)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Debug.Trace (trace)
import Fmt

traceShow :: Show a1 => a1 -> a2 -> a2
traceShow x = trace (show x)

traceShowId :: Show a1 => String -> a1 -> a1
traceShowId label x = trace (label |+ " " +|| x ||+ "") x

promptRun :: T.Text -> IO () -> IO Bool
promptRun message action = do
  TIO.putStrLn $ fmt "Run " +| message |+ "?"
  response <- getChar
  if T.pack [response] == "y"
    then do
      TIO.putStrLn $ fmt "Running " +| message |+ ""
      action
      return True
    else do
      TIO.putStrLn $ fmt "Skipping " +| message |+ ""
      return False

assertIsEqual :: (Eq a, Show a) => a -> a -> IO ()
assertIsEqual x y = do
  assertIsEqualBase True x y

assertIsEqualSilent :: (Eq a, Show a) => a -> a -> IO ()
assertIsEqualSilent x y = do
  assertIsEqualBase False x y

assertIsEqualBase :: (Eq a, Show a) => Bool -> a -> a -> IO ()
assertIsEqualBase showMessage x y = do
  unless (x == y) $ error $ fmtLn $ "assertIsEqual:" +|| x ||+ "!=" +|| y ||+ ""
  when showMessage $ fmtLn $ "assertIsEqual:" +|| x ||+ "==" +|| y ||+ ""

printBannerWrap :: T.Text -> IO () -> IO ()
printBannerWrap name io = printBanner (name |+ " start") >> io >> printBanner (name |+ " end")

printBanner :: T.Text -> IO ()
printBanner name = fmtLn $ "=====" +| name |+ "====="

infixl 9 .@, ..@

(.@) :: (t1 -> t2 -> t3) -> t2 -> t1 -> t3
f .@ b = \a -> f a b

(..@) :: (t1 -> t2 -> t3 -> t4) -> t3 -> t1 -> t2 -> t4
f ..@ c = \a b -> f a b c

whileLoop :: Monad m => m Bool -> m () -> m ()
whileLoop mpred mcall = mpred >>= when .@ (mcall >> whileLoop mpred mcall)

pauseIO :: IO ()
pauseIO = do
  putStrLn "Press any key to continue"
  _ <- getChar
  putStrLn ""

type Message = String

type TestState = StateT [Message] (State (IO ())) ()

runTest :: TestState -> IO ()
runTest tests = do
  let inner = runStateT tests []
      (((), messages), io) = runIdentity $ runStateT inner (pure ())
   in do
        io
        putStrLn $
          ""
            +| nameF "tests" (build $ intercalate ", " messages)
            |+ "\n"
            +| nameF "count" (build $ length messages)
            |+ "\n"
            +| "success"

useIO :: IO () -> TestState
useIO next = lift $ modify (>> next)

createTest :: IO () -> Message -> TestState
createTest x message = do
  useIO
    ( do
        when (null message) (error "Missing name")
        printBanner $ "start:" +| message |+ ""
        x
        printBanner $ "end:" +| message |+ ""
    )
  modify (++ [message])

createChapterTest :: Message -> Message -> IO () -> TestState
createChapterTest chapter msg test = createTest test $ "Chapter " +| chapter |+ ":" +| msg |+ ""

wrapTest :: TestState -> Message -> TestState
wrapTest tests message = do
  useIO $ printBanner $ "start:" +| message |+ ""
  tests
  useIO $ printBanner $ "end:" +| message |+ ""

testDone :: IO ()
testDone = pure ()
