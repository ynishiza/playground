{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Chapter7_2 (run) where

import Control.Monad.Except
import Data.Functor.Identity
import Control.Monad.State
import Control.Monad.Trans.Except hiding (tryE, finallyE)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Fmt
import Utils

run :: TestState
run =
  createChapterTest
    "7.2"
    "MonadError"
    ( do
        runTest $ do 
          testErrorHandlingInNestedExceptT 
          testExceptionHandlers
        testDone
    )

data MyError = MySimpleError !Text | MyFatalError !Text deriving (Show, Eq)

getErrorText :: MyError -> Text
getErrorText (MySimpleError m) = m
getErrorText (MyFatalError m) = m

instance Buildable MyError where
  build e = e ||+ ""

instance Buildable () where
  build = build . show

instance (Buildable a, Buildable b) => Buildable (a, b) where
  build (x, y) = "(" +| x |+ "," +| y |+ ")"

instance (Buildable a, Buildable b) => Buildable (Either a b) where
  build (Left x) = "Left " +| x |+ ""
  build (Right x) = "Right " +| x |+ ""

type MySimpleComp = StateT Double (ExceptT MyError IO)

type MySimpleCompResult a = Either MyError (a, Double)

tryE :: Monad m => ExceptT e m a -> ExceptT e m (Either e a)
tryE m = catchE (Right <$> m) (return . Left)

finallyE :: Monad m => ExceptT e m a -> ExceptT e m () -> ExceptT e m a
finallyE m closer = do
  res <- tryE m
  closer
  either throwE return res

testErrorHandlingInNestedExceptT :: TestState
testErrorHandlingInNestedExceptT =
  createTest
    ( do
        let smult :: Double -> MySimpleComp ()
            smult n = slog ("*" +| n |+ "") >> modify (* n)
            sadd :: Double -> MySimpleComp ()
            sadd n = slog ("+" +| n |+ "") >> modify (+ n)
            sdiv :: Double -> MySimpleComp ()
            sdiv n
              | n == 0 = l >> slogAndError "Division by zero!"
              | otherwise = l >> modify (/ n)
              where
                l = slog ("/" +| n |+ "")

            slog :: Text -> MySimpleComp ()
            slog m = do
              v <- get
              liftIO $ TIO.putStrLn $ "log[" +| v |+ "]: " +| m |+ ""

            slogAndError :: Text -> MySimpleComp ()
            slogAndError m = slog ("ERROR:" +| m |+ "") >> throwError (MySimpleError m)
            catchAndReset :: MySimpleComp () -> Double -> MySimpleComp ()
            catchAndReset c v = catchError c handle
              where
                handle e = slog ("caught error" +| getErrorText e |+ "") >> put v
            catchAndRethrowAs :: MySimpleComp () -> (Text -> MyError) -> MySimpleComp ()
            catchAndRethrowAs c f = catchError c handle
              where
                handle e = slog ("caught error" +| getErrorText e |+ "") >> throwError (f (getErrorText e))

            runMySimpleComp :: Double -> MySimpleComp a -> IO (MySimpleCompResult a)
            runMySimpleComp v = runExceptT . (`runStateT` v)
            printMySimpleComp :: Buildable a => Double -> MySimpleComp a -> IO (MySimpleCompResult a)
            printMySimpleComp v c = do
              res <- runMySimpleComp v c
              TIO.putStrLn $ pretty res
              return res

            withError :: MySimpleComp a -> (MyError -> MyError) -> MySimpleComp a
            withError (StateT c) f = StateT $ withExceptT f <$> c

            assertMySimpleCompv :: (Eq a, Show a, Buildable a) => MySimpleCompResult a -> Double -> MySimpleComp a -> IO ()
            assertMySimpleCompv expected v c = do
              res <- printMySimpleComp v c
              assertIsEqual expected res

        printBanner "Compute test"
        assertMySimpleCompv
          (Right (201, 200))
          0
          ( do
              sadd 1
              sadd 1
              smult 10
              smult 10
              gets (+ 1)
          )

        printBanner "catchError"
        let compute1 = do
              sadd 1
              sdiv 0
            compute2 = do
              sadd 10
              smult 2
         in do
              assertMySimpleCompv
                (Left (MySimpleError "Division by zero!"))
                0
                ( do
                    compute1
                    compute2
                    get
                )
              assertMySimpleCompv
                (Right ((), -180))
                0
                ( do
                    catchAndReset compute1 (-100)
                    compute2
                )
              assertMySimpleCompv
                (Left (MyFatalError "RETHROW: Division by zero!"))
                0
                ( do
                    catchAndRethrowAs compute1 (MyFatalError . ("RETHROW: " `T.append`))
                    compute2
                )
              assertMySimpleCompv
                (Left (MyFatalError "Division by zero!"))
                0
                ( do
                    withError compute1 (MyFatalError . getErrorText)
                    compute2
                )
    )
    "testErrorHandlingInNestedExceptT"

testExceptionHandlers :: TestState
testExceptionHandlers = createTest (do
        let
          e1 :: Monad m => ExceptT String m Int
          e1 = throwE "OOOOPS"
          e2 :: Monad m => ExceptT String m Int
          e2 = throwE "BAD"
          v1 :: Monad m => ExceptT a m Int
          v1 = return 1
          finallyHandler :: ExceptT a IO ()
          finallyHandler = liftIO $ putStrLn "finallyHandler"
          runAndAssert :: (Show e, Show a, Eq e, Eq a) => Either e a -> Except e a -> IO ()
          runAndAssert expected = assertIsEqual expected . runExcept
          runAndAssertIO :: (Show e, Show a, Eq e, Eq a) => Either e a -> ExceptT e IO a -> IO ()
          runAndAssertIO expected c = do 
            v <- runExceptT c
            assertIsEqual expected v


          in do
            runAndAssert (Left "OOOOPS") e1
            runAndAssert (Left "aOOOOPS") (withExceptT ("a" ++) e1)
            runAndAssert (Right 1) (withExceptT ("a" ++) v1)
            (runAndAssert @String @Int) (Right 100) (catchE e1 (const (return 100)))
            (runAndAssert @String @Int) (Right 1) (catchE v1 (const (return 100)))
            runAndAssert (Left "BAD") (catchE e1 $ const e2)
            runAndAssert (Right 1) (catchE v1 $ const e2)
            runAndAssert (Right (Right 1)) (tryE @Identity @String v1)
            runAndAssert (Right (Left "OOOOPS")) (tryE e1)
            runAndAssertIO (Right 1) (finallyE @IO @String @Int (return 1) finallyHandler)
            runAndAssertIO (Left "OOOOPS") (finallyE @IO @String @Int (throwE "OOOOPS") finallyHandler)
        testDone
  ) "testExceptionHandlers"

