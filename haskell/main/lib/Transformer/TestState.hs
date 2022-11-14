{-# LANGUAGE OverloadedStrings #-}
{-# HLINT ignore "Use const" #-}
{-# HLINT ignore "Use tuple-section" #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Transformer.TestState
  ( testStateWithAndWithoutMonads,
    testReadState,
    testNestedState,
    testWriteState,
    testCont,
    testContWithIO,
    testDelimitedCont,
  )
where

import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Cont
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State.Lazy
import Control.Monad.Trans.Writer.Lazy
import Data.Char
import Data.Foldable
import Fmt
import TestUtils

data User = User {name :: String, count :: Int} deriving (Show, Eq)

type ComputeStateData = ([Int], IO ())

testCont :: TestState
testCont =
  createTest
    ( do
        let cmult :: Int -> Int -> Cont a Int
            cmult y x = cont (\ret -> ret $ y * x)
            cfact :: Int -> Cont a Int
            cfact 1 = cont ($ 1)
            cfact n = cont (\ret -> ret $ n * evalCont inner)
              where
                inner = cfact (n - 1)

            cTask = do
              x <- cfact 5
              y <- cmult x 2
              return $ traceShowId "pair" (x, y)
         in -- return $ x + y
            --
            do
              runCont cTask print
        -- runCont reset0 $ \x -> print $ 10 + x

        printBanner "Short circuit test"
        let evenCheckCC :: Int -> Cont r String
            evenCheckCC n =
              callCC
                ( \k -> do
                    when (even n) $ k "even"
                    trace "end reached" $ return "odd"
                )
            evenCheckThrow n = do
              when (even n) $ cont (\_ -> "even")
              trace "endReached" return "odd"
            evenCheckAlwaysOdd n = do
              _ <- if even n then return "even" else return ""
              trace "end reached" return "odd"

            shortCircuitCC =
              callCC
                ( \k -> do
                    _ <- k (-1)
                    return 10
                )
            shortCircutAll :: Cont Int String
            shortCircutAll = do
              _ <- cont (\_ -> (-1))
              return ""

            nextCont = ("result:" ++)
         in do
              print $ runCont (evenCheckCC 2) nextCont
              print $ runCont (evenCheckCC 1) nextCont
              print $ runCont (evenCheckThrow 2) nextCont
              print $ runCont (evenCheckThrow 1) nextCont
              print $ runCont (evenCheckAlwaysOdd 2) nextCont
              print $ runCont (evenCheckAlwaysOdd 1) nextCont
              print $ runCont shortCircuitCC (+ 100)
              print $ runCont shortCircutAll ((+ 100) . read)

        printBanner "base test"
        let f1 x = x + x
            f2 x = x * x
            f3 x = x - 1
            c0 :: Cont r Int
            c0 = return 10
            k1 n = return $ traceShowId "f1 n" (f1 n)
            k2 n = return $ traceShowId "f2 n" (f2 n)
            k3 n = return $ traceShowId "f3 n" (f3 n)
            chainCont :: Cont r a -> (a -> Cont r b) -> Cont r b
            chainCont c1 c2 = cont (\ret -> runCont c1 (\x -> runCont (c2 x) ret))
            comb c = c `chainCont` k1 `chainCont` k2 `chainCont` k3
            comb2 c = c >>= k1 >>= k2 >>= k3
            comb3 c = do
              x <- c
              x1 <- k1 x
              x2 <- k2 x1
              k3 x2

            finalize x = "result:" ++ show x
         in do
              putStrLn $ runCont (comb c0) finalize
              putStrLn $ runCont (comb2 c0) finalize
              putStrLn $ runCont (comb3 c0) finalize
              putStrLn $ runCont (comb3 $ return 100) finalize
              putStrLn $ runCont (comb3 $ return 100) finalize
        -- putStrLn $ runCont (c1 >>= return.f1 >>= return.f2) show
        testDone
    )
    "testCont"

type ContResult = StateT [Int] IO

type IOCont r = ContT r ContResult

testContWithIO :: TestState
testContWithIO =
  createTest
    ( do
        let cmult :: Int -> Int -> IOCont r Int
            cmult x y = do
              let result = x * y
              lift
                ( do
                    modify (result :)
                    lift $ putStrLn $ fmtLn "x" +| x |+ " y=" +| y |+ " result=" +| result |+ ""
                    return result
                )

            crequireLt :: Int -> IOCont r Int -> IOCont r Int
            crequireLt upper inner =
              callCC
                ( \k -> do
                    x <- inner
                    let resetValue = 0
                        resetState = do
                          modify (resetValue :)
                          lift $ fmtLn $ x |+ ">" +| upper |+ "  reset=" +| resetValue |+ ""
                          return ()

                    if x > upper
                      then do
                        lift resetState
                        k resetValue
                      else return x
                )

            cSomeComputation n = do
              r1 <- cmult n n
              r2 <- cmult n r1
              r3 <- cmult r1 r2
              crequireLt 1000 $ cmult r2 r3

            runComputeWith :: IOCont Int Int -> IO ()
            runComputeWith c = do
              let st = runContT c return
                  res = runStateT st []
              do
                (computeRes, computeHistory) <- res
                fmtLn $ "computeRes=" +| computeRes |+ "  state=" +| computeHistory |+ ""

        traverse_ (runComputeWith . cSomeComputation) [1 .. 10]
        testDone
    )
    "testContWithIO"

instance (Show a, Show b) => Buildable (a, b) where
  build = build . show

testDelimitedCont :: TestState
testDelimitedCont =
  createTest
    ( do
        let reset0 :: Cont r Int
            reset0 = reset $ return 0
            reset1 :: Cont r Int
            reset1 = reset $ do
              shift (\f -> return $ f 1)
            reset2 :: Cont r Int
            reset2 = reset $ do
              x <- shift (\f -> return $ f 1)
              return $ 2 * x
            reset3 :: Cont r Int
            reset3 = reset $ do
              x <- shift (\f -> return $ f $ f 1)
              return $ 2 * x

            binarySequences :: Int -> Cont r [[Int]]
            binarySequences n = reset $ do
              x <-
                shift $
                  return . foldr ($) [[1], [0]] . replicate n
              return $ do
                a <- x
                [a, 0 : a, 1 : a]

            reset5 :: Cont r Int
            reset5 = reset $ do
              x <- shift (\ret -> return $ ret $ ret $ ret 1)
              return $ x * 10
            reset5raw :: Cont r Int
            reset5raw = reset $ do
              x <- cont (\ret -> ret $ ret $ ret 1)
              return $ x * 10
            reset5T :: ContT r Maybe Int
            reset5T = resetT $ do
              x <- shiftT (\ret -> lift $ Just 1 >>= ret >>= ret >>= ret)
              lift $ Just (x * 10)
            reset5TRaw :: ContT r Maybe Int
            reset5TRaw = resetT $ do
              x <- callN
              lift $ Just (x * 10)
              where
                callN = ContT (\ret -> Just 1 >>= ret >>= ret >>= ret)

            f1 :: Int -> Cont r Int
            f1 x = reset $ do
              x' <- shift (\ret -> return $ ret $ ret x)
              return $ 2 * x' + 1
            f1Delimit x = do
              y <- reset $ do
                x' <- shift (\ret -> return $ ret $ ret x)
                return $ 2 * x'
              return $ y + 1

            gyield :: Functor f => f a -> (f r -> r) -> Cont r a
            gyield list handleResult =
              shift
                ( \ret -> do
                    let x = ret <$> list
                     in return $ handleResult x
                )

            generatorTask :: [Int] -> Cont r Int
            generatorTask list = do
              res <- reset $ do
                x <- gyield list sum
                return $ traceShowId "gen" $ 10 * x
              return $ res + 1

            useReset c = do
              x <- c
              return $ x * 2

            runReset :: Buildable a => Cont (IO ()) a -> IO ()
            runReset r = runCont r (fmtLn . ("result:" +|) . build)
            runResetMany :: Buildable a => [Cont (IO ()) a] -> IO ()
            runResetMany = traverse_ runReset
         in do
              runResetMany $
                fmap binarySequences [0 .. 4]
              runResetMany
                [ reset0,
                  reset1,
                  reset2,
                  reset3
                ]

              printBanner "Shift + reset"
              runResetMany
                [ reset5,
                  useReset reset5,
                  reset5raw,
                  useReset reset5raw
                ]
              print $ runContT reset5T return
              print $ runContT reset5TRaw return
              runResetMany
                [ f1 1,
                  f1 2,
                  f1Delimit 1,
                  f1Delimit 2
                ]

              printBanner "Generator"
              runReset $ generatorTask [1 .. 10]
        testDone
    )
    "testDelimitedCont"

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

testReadState :: TestState
testReadState =
  createTest
    ( do
        printBanner "Reader"
        let readTask :: Reader [Int] Int
            readTask = do
              list <- ask
              v1 <- asks (!! 0)
              v2 <- asks (!! 1)
              return $ v1 + v2 + last list

            readTask2 = local ((* 2) <$>) readTask

            printReader :: Show a => Reader r a -> r -> IO ()
            printReader r readOnlyState =
              let result = runReader r readOnlyState
               in putStrLn $ prependLabel "result" result

        printReader readTask [1 .. 10]
        printReader readTask2 [1 .. 10]

        testDone
    )
    "testReadState"

testWriteState :: TestState
testWriteState =
  createTest
    ( do
        let task1 :: Writer String Int
            task1 = do
              tell "hello"
              tell "world"

              _ <- listen (return ())
              return 1

            wToUpper :: Writer String a -> Writer String a
            wToUpper task = pass $ do
              v <- task
              return (v, (toUpper <$>))

            wToLower :: Writer String a -> Writer String a
            wToLower = censor (toLower <$>)

            writeLog :: Writer String a -> Writer String (IO ())
            writeLog task = do
              (_, w) <- listen task
              return $ putStrLn $ "log:" ++ w

            printWriter :: (Show w, Show a) => Writer w a -> IO ()
            printWriter w =
              let (_state, result) = runWriter w
               in putStrLn $ prependLabel "state" _state ++ prependLabel " result" result

        printBanner "Writer"
        printWriter task1
        printWriter (wToUpper task1)
        printWriter (wToLower task1)
        printWriter (wToLower (tell "Test"))
        printWriter (listen (tell "Test"))
        printWriter $ pass $ (\x -> (x, reverse)) <$> tell "hello"
        printWriter $ pass $ tell "hello" >> return ((), reverse)

        fst $ runWriter (writeLog task1)
        fst $ runWriter (writeLog $ wToUpper task1)
        testDone
    )
    "testWriteState"

type MyNestedState = StateT Int (StateT String (State (IO ()))) ()

type MyNestedStateIO = StateT Int (StateT String IO) ()

-- Reference: http://blog.sigfpe.com/2006/05/grok-haskell-monad-transformers.html
testNestedState :: TestState
testNestedState =
  createTest
    ( do
        let task :: MyNestedState
            task = do
              put 1
              modify (* 10)
              x <- get

              lift $ put $ prependLabel "x" x
              lift $ modify ("Result: " ++)
              msg <- lift get

              let io' = do
                    putStrLn msg
                    putStrLn "Press any key"
                    _ <- getChar
                    _ <- getChar
                    putStrLn ""
               in lift $ lift $ modify (>> io')
              return ()
            r1 = runStateT task 1
            r2 = runStateT r1 ""
            (((_, v), s), io) = runState r2 (pure ())
         in do
              printBanner "MyNestedState"
              putStrLn "Final result"
              putStrLn $ prependLabel "value" v
              putStrLn $ prependLabelStr "string" s
              putStrLn "io ="
              io

        let task :: MyNestedStateIO
            task = do
              lift $ lift $ putStrLn "Hello"
              put 1
              lift $ put "This is a message"
            r1 = runStateT (runStateT task 1) ""
         in do
              printBanner "MyNestedStateIO"
              ((_, v), s) <- r1
              putStrLn $ prependLabelStr "string" s
              putStrLn $ prependLabel "value" v

        testDone
    )
    "testNestedState"
