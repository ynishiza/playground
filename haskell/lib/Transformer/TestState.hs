{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use const" #-}
module Transformer.TestState
  ( testStateWithAndWithoutMonads,
    testReadState,
    testNestedState,
    testWriteState,
    testCont,
  )
where

import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Cont
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State.Lazy
import Control.Monad.Trans.Writer.Lazy
import Data.Char
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
              traceShow x (pure ())
              traceShow y (pure ())
              return (x, y)
            -- return $ x + y
            --

            reset0 = reset (return 0)
         in do
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
            f4 x = (-x)
            c1 :: Cont r Int
            c1 = return 10
            k1 n = return $ trace (show n) (f1 n)
            k2 n = return $ trace (show n) (f2 n)
            k3 n = return $ trace (show n) (f3 n)
            chainCont :: Cont r a -> (a -> Cont r b) -> Cont r b
            chainCont c1 c2 = cont (\ret -> runCont c1 (\x -> runCont (c2 x) ret))
            comb c = c `chainCont` k1 `chainCont` k2 `chainCont` k3
            comb2 c = c >>= k1 >>= k2 >>= k3
            comb3 c = do
              x <- c
              x1 <- k1 x
              x2 <- k2 x1
              k3 x2

            finalize x = "result:" ++ (show x)
         in do
              putStrLn $ runCont (comb c1) finalize
              putStrLn $ runCont (comb2 c1) finalize
              putStrLn $ runCont (comb3 c1) finalize
              putStrLn $ runCont (comb3 $ return 100) finalize
              putStrLn $ runCont (comb3 $ return 100) finalize
        -- putStrLn $ runCont (c1 >>= return.f1 >>= return.f2) show
        testDone
    )
    "testCont"

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

              m <- listen (return ())
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
