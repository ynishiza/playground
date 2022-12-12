{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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
    assertIsEqualType,
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
    UtilErrors (..),
    shouldNeverHappen,
    shouldNeverHappenIO,
    showBuilder,
  )
where

import Control.Exception
import Control.Monad
import Control.Monad.State
import Data.List (intercalate)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Typeable
import Debug.Trace (trace)
import Fmt

data UtilErrors = ShouldNeverHappen | NotImplemented deriving (Show)

instance Exception UtilErrors where

shouldNeverHappen :: a
shouldNeverHappen = throw ShouldNeverHappen

shouldNeverHappenIO :: IO a
shouldNeverHappenIO = throwIO ShouldNeverHappen

showBuilder :: Show a => a -> Builder
showBuilder = build . show

traceShow :: Show a1 => a1 -> a2 -> a2
traceShow x = trace (show x)

traceShowId :: Show a1 => String -> a1 -> a1
traceShowId label x = trace (label |+ " " +|| x ||+ "\n") x

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
assertIsEqual = assertIsEqualWithShow True

assertIsEqualType :: (Typeable a, Typeable b) => a -> b -> IO ()
assertIsEqualType x y = assertIsEqualWithShow True (typeOf x) (typeOf y)

assertIsEqualSilent :: (Eq a, Show a) => a -> a -> IO ()
assertIsEqualSilent = assertIsEqualWithShow False

assertIsEqualWithShow :: (Eq a, Show a) => Bool -> a -> a -> IO ()
assertIsEqualWithShow showMessage x y = assertIsEqualBase showMessage x (show x) y (show y)

assertIsEqualBase :: (Eq a) => Bool -> a -> String -> a -> String -> IO ()
assertIsEqualBase showMessage x xm y ym = do
  unless (x == y) $ error $ fmtLn $ "assertIsEqual:" +|| xm ||+ "!=" +|| ym ||+ ""
  when showMessage $ fmtLn $ "assertIsEqual:" +|| xm ||+ "==" +|| ym ||+ ""

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
pauseIO =
  putStrLn "Press any key to continue"
    >> getChar
    >> putStrLn ""

type Message = String

newtype TestStateBase a = TestState {runTestState :: StateT [Message] IO a}
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadIO,
      MonadState [Message]
    )

type TestState = TestStateBase ()

runTest :: TestState -> IO ()
runTest tests = do
  let inner = runStateT (runTestState tests) []
   in do
        ((), messages) <- inner
        putStrLn
          ( ""
              +| nameF "tests" (build $ intercalate ", " messages)
              |+ "\n"
              +| nameF "count" (build $ length messages)
              |+ "\n"
              +| "success"
          )

useIO :: IO () -> TestState
useIO = liftIO

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
