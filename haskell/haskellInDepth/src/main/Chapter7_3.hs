{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Chapter7_3
  ( run,
    divPure,
    divPureIO,
    divPureM,
    catchSample,
    finallySample,
    bracketSample,
    catchByTypeTest,
    MyError,
  )
where

import Control.Exception
import Control.Monad
import qualified Control.Monad.Catch as ME
import Data.Foldable
import qualified Data.Text as T
import Fmt
import Utils

data MyError = DivZero | AnotherError deriving (Show)

instance Exception MyError

instance Buildable () where
  build = build . show

catchFallbackTo :: Exception e => String -> a -> e -> IO a
catchFallbackTo label v e = ("[" +| label |+ "] catch fallback on error:" +|| e ||+ "\n") >> return v

catchFallbackTo0All :: SomeException -> IO Int
catchFallbackTo0All = catchFallbackTo0

catchFallbackTo0 :: Exception e => e -> IO Int
catchFallbackTo0 = catchFallbackTo "" 0

tryFallbackTo :: Exception e => String -> a -> Either e a -> IO a
tryFallbackTo label d (Left e) = ("[" +| label |+ "] try fallback on error:" +|| e ||+ "\n") >> return d
tryFallbackTo _ _ (Right v) = return v

tryFallbackTo0 :: Exception e => Either e Int -> IO Int
tryFallbackTo0 = tryFallbackTo "" 0

alwaysCatch :: IO b -> IO ()
alwaysCatch m = handle (catchFallbackTo @SomeException "alwaysCatch" ()) (Control.Monad.void m)

run :: TestState
run =
  createChapterTest
    "7.3"
    "GHC exceptions"
    $ handle
      (catchFallbackTo @SomeException "top" ())
      ( do
          let printMany :: Buildable a => String -> [IO a] -> IO ()
              printMany name x = do
                printBanner $ "start:" +| name |+ ""
                traverse_ (>>= prettyLn) x
                printBanner $ "end:" +| name |+ ""

          printMany
            "catch @SomeException i.e. every error"
            [ catch (do let !x = badFold in return 30) catchFallbackTo0All,
              catch (evaluate badFold) catchFallbackTo0All,
              catch (evaluate badDivNative) (catchFallbackTo0 @SomeException),
              catch (evaluate badDivPure) (catchFallbackTo0 @SomeException),
              catch (evaluate badDivPureText1) (catchFallbackTo0 @SomeException),
              catch (evaluate badDivPureTextOnRead) (catchFallbackTo0 @SomeException),
              catch badDivPureIO (catchFallbackTo0 @SomeException),
              catch badDivPureM (catchFallbackTo0 @SomeException)
            ]
          printMany
            "catch @MyError i.e. MyError only"
            [ -- handle (catchFallbackTo0 @MyError) (evaluate badFold),            Not MyError
              -- handle (catchFallbackTo0 @MyError) (evaluate badDivNative),       Not MyError
              catch (evaluate badDivPure) (catchFallbackTo0 @MyError),
              catch (evaluate badDivPureText1) (catchFallbackTo0 @MyError),
              -- handle (catchFallbackTo0 @MyError) (evaluate badDivPureTextOnRead),    Not MyError
              catch badDivPureIO (catchFallbackTo0 @MyError),
              catch badDivPureM (catchFallbackTo0 @MyError)
            ]

          printMany
            "catches multiple i.e. every error"
            [ catches
                (evaluate badDivNative)
                [ Handler (catchFallbackTo0 @Deadlock),
                  Handler (catchFallbackTo0 @MyError),
                  Handler (catchFallbackTo0 @ArithException),
                  Handler (\(e :: MyError) -> throwIO e)
                ]
            ]

          let

          printMany
            "try @SomeException i.e. every error"
            [ try @SomeException (evaluate badFold) >>= tryFallbackTo0,
              try @SomeException (evaluate badDivNative) >>= tryFallbackTo0,
              try @SomeException (evaluate badDivPure) >>= tryFallbackTo0,
              try @SomeException (evaluate badDivPureText1) >>= tryFallbackTo0,
              try @SomeException (evaluate badDivPureTextOnRead) >>= tryFallbackTo0,
              try @SomeException badDivPureIO >>= tryFallbackTo0,
              try @SomeException badDivPureM >>= tryFallbackTo0
            ]

          printMany
            "try @MyError i.e. MyError only"
            [ -- try @MyError (evaluate badFold) >>= th0,
              -- try @MyError (evaluate badDivNative) >>= th0,
              try @MyError (evaluate badDivPure) >>= tryFallbackTo0,
              try @MyError (evaluate badDivPureText1) >>= tryFallbackTo0,
              -- try @MyError (evaluate badDivPureTextOnRead) >>= th0,
              try @MyError badDivPureIO >>= tryFallbackTo0,
              try @MyError badDivPureM >>= tryFallbackTo0
            ]

          printMany
            "tryJust @SomeException"
            [ tryJust @SomeException Just (evaluate badFold) >>= tryFallbackTo0,
              tryJust @SomeException Just (evaluate badDivNative) >>= tryFallbackTo0,
              tryJust @SomeException Just (evaluate badDivPure) >>= tryFallbackTo0,
              tryJust @SomeException Just (evaluate badDivPureText1) >>= tryFallbackTo0,
              tryJust @SomeException Just (evaluate badDivPureTextOnRead) >>= tryFallbackTo0,
              tryJust @SomeException Just badDivPureIO >>= tryFallbackTo0,
              tryJust @SomeException Just badDivPureM >>= tryFallbackTo0
            ]

          printMany
            "try @MyError"
            [ -- tryJust @MyError Just (evaluate badFold) >>= tryFallbackTo0,
              -- tryJust @MyError Just (evaluate badDivNative) >>= tryFallbackTo0,
              tryJust @MyError Just (evaluate badDivPure) >>= tryFallbackTo0,
              tryJust @MyError Just (evaluate badDivPureText1) >>= tryFallbackTo0,
              -- tryJust @MyError Just (evaluate badDivPureTextOnRead) >>= tryFallbackTo0,
              tryJust @MyError Just badDivPureIO >>= tryFallbackTo0,
              tryJust @MyError Just badDivPureM >>= tryFallbackTo0
            ]

          printMany
            "finally"
            [ alwaysCatch $ finally deadlock (putStrLn "FINALLY"),
              alwaysCatch $ finally (putStrLn "Success") (putStrLn "FINALLY"),
              alwaysCatch $ onException deadlock (putStrLn "ONEXCEPTION"),
              alwaysCatch $ onException (putStrLn "Success") (putStrLn "ONEXCEPTION")
            ]

          let printValue :: Buildable a => T.Text -> a -> IO a
              printValue label x = (label |+ " - value:" +| x |+ "\n") >> return x
          printMany
            "bracket"
            [ alwaysCatch $ bracket (printValue @Int "start" 1) (printValue "end") (printValue "process"),
              alwaysCatch $ bracket (deadlock >> return 2) (printValue @Int "end") (printValue "process"),
              alwaysCatch $ bracket (printValue @Int "start" 3) (printValue "end") (const deadlock)
            ]

          printBanner ""
          alwaysCatch task1
          printBanner "catch sample"
          alwaysCatch catchSample
          printBanner "finally sample"
          alwaysCatch finallySample
          printBanner "bracket sample"
          alwaysCatch bracketSample
          putStrLn "Done"
          testDone
      )

deadlock :: IO a
deadlock = throwIO Deadlock

badFold :: a
badFold = foldr1 (\_ y -> y) []

badDivNative :: Int
badDivNative = 2 `div` 0

badDivPure :: Int
badDivPure = divPure 2 0

badDivPureText1 :: Int
badDivPureText1 = divPureText "2" "0"

badDivPureTextOnRead :: Int
badDivPureTextOnRead = divPureText "2" "a"

badDivPureIO :: IO Int
badDivPureIO = divPureIO 2 0

badDivPureM :: ME.MonadCatch m => m Int
badDivPureM = divPureM 2 0

divPureText :: String -> String -> Int
divPureText x y = divPure (read x) (read y)

divPure :: Int -> Int -> Int
divPure _ 0 = throw DivZero
divPure x y = div x y

divPureIO :: Int -> Int -> IO Int
divPureIO _ 0 = throwIO DivZero
divPureIO x y = pure $ divPure x y

divPureM :: ME.MonadCatch m => Int -> Int -> m Int
divPureM _ 0 = ME.throwM DivZero
divPureM x y = pure $ divPure x y

-- computeWithTryIO :: Int -> (a -> a -> IO Int) -> a -> a -> IO Int
-- computeWithTryIO def f x y = E.try @MyError (f x y) >>= pure . g
--   where
--     g (Left _) = def
--     g (Right n) = n

-- computeWithTryJustIO :: Int -> (a -> a -> IO Int) -> a -> a -> IO Int
-- computeWithTryJustIO def f x y = E.tryJust @MyError h (f x y) >>= pure . g
--   where
--     h DivZero = Just ()
--     h AnotherError = Just ()
--     g (Left _) = def
--     g (Right n) = n

task1 :: IO ()
task1 = do
  let fallback :: SomeException -> IO Int
      fallback e = print e >> return 0

  catch (evaluate (div 1 0)) fallback >>= print
  -- catch (pure (div 1 0)) fallback >>= print
  pure ()

catchSample :: IO ()
catchSample = do
  let c1 = return 100
      c2 = throwIO Deadlock

  let logAndSuppress :: SomeException -> IO Int
      logAndSuppress e = print e >> return 0

      logAndRethrow :: SomeException -> IO Int
      logAndRethrow e = print e >> throwIO e

  c1 `catch` logAndSuppress >>= print
  c2 `catch` logAndSuppress >>= print
  c1 `catch` logAndRethrow >>= print
  -- c2 `catch` logAndRethrow >>= print

  printBanner "try"
  let logAndSuppress2 :: Either SomeException Int -> IO Int
      logAndSuppress2 (Left e) = print e >> return (-1)
      logAndSuppress2 (Right v) = return v
      logAndRethrow2 :: Either SomeException Int -> IO Int
      logAndRethrow2 (Left e) = print e >> throwIO e
      logAndRethrow2 (Right v) = return v

  try c1 >>= logAndSuppress2
    >>= print
  try c2 >>= logAndSuppress2
    >>= print
  try c1 >>= logAndRethrow2
    >>= print
  -- try c2 >>= logAndRethrow2  >>= print

  putStrLn "Done"
  pure ()

finallySample :: IO ()
finallySample = do
  let cerror = throwIO @Deadlock @Int Deadlock

  putStrLn "start" `finally` putStrLn "finally"
  -- finally (cerror) (putStrLn "finally")
  putStrLn "start" `onException` putStrLn "onException"
  -- onException (cerror) (putStrLn "onException")

  let onError :: SomeException -> IO Int
      onError e = print e >> return 0
  cerror `finally` putStrLn "finally"
    `catch` onError
    >>= print
  return 10 `finally` putStrLn "finally"
    `catch` onError
    >>= print

  pure ()

bracketSample :: IO ()
bracketSample = do
  let printValue :: Buildable a => T.Text -> a -> IO a
      printValue label x = (label |+ " - value:" +| x |+ "\n") >> return x
      cstart = printValue @Int "start" 0
      cfinally = printValue "finally"
      cprocess = printValue "main"
      cerror = throwIO @Deadlock @Int Deadlock

  bracket cstart cfinally cprocess >>= print
  bracket cstart cfinally (const cerror) >>= print
  bracket cerror cfinally cprocess >>= print

  pure ()

handleGeneric1 :: SomeException -> IO ()
-- handleGeneric1 DivideByZero = putStrLn "Divide"
handleGeneric1 e = putStrLn $ "handleGeneric1:" ++ show e

handleGeneric2 :: Exception e => e -> IO ()
-- handleGeneric2 DivideByZero = putStrLn "Divide"
handleGeneric2 e = putStrLn $ "handleGeneric2:" ++ show e

handleAsync :: AsyncException -> IO ()
handleAsync StackOverflow = putStrLn "Overflow!"
handleAsync HeapOverflow = putStrLn "Heap!"
handleAsync e = putStrLn $ "Other async exception:" ++ show e

handleArith :: ArithException -> IO ()
handleArith DivideByZero = putStrLn "Zero!"
handleArith LossOfPrecision = putStrLn "Precision!"
handleArith e = putStrLn $ "Other arith exception:" ++ show e

rethrowArith :: ArithException -> IO ()
rethrowArith e = putStrLn ("rethrowArith:" ++ show e) >> throwIO e

rethrowGeneric1 :: SomeException -> IO ()
rethrowGeneric1 e = putStrLn ("rethrowGeneric1:" ++ show e) >> throwIO e

catchByTypeTest :: IO ()
catchByTypeTest = do
  let handlers =
        [ Handler handleAsync,
          Handler handleArith,
          Handler handleGeneric1,
          Handler (handleGeneric2 @SomeException)
        ]
  throw DivideByZero `catches` handlers
  throw LossOfPrecision `catches` handlers
  throw Denormal `catches` handlers
  throw StackOverflow `catches` handlers
  throw Deadlock `catches` handlers

  (throw DivideByZero `catch` rethrowArith) `catch` rethrowGeneric1
  throw DivideByZero `catches` [Handler rethrowArith, Handler rethrowGeneric1]

  putStrLn "Done"
  return ()
