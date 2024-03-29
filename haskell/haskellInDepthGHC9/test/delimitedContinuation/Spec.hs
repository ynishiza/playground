{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TupleSections #-}

module Spec
  ( spec,
  )
where

import ContState qualified as CS
import Control.Monad
import Control.Monad.IO.Class
import CtT
import Data.Coerce
import Data.Foldable
import Data.Function
import Data.List (sort)
import Data.Monoid
import State
import System.Directory
import System.FilePath
import System.IO
import System.IO.Extra
import System.PosixCompat
import Test.Hspec
import Tree

i :: Int -> Int
i = id

infixr 4 +#, -#, ++#

infixr 5 *#

infixl 4 #+, #-, #++

infixl 5 #*

(+#) :: Num a => a -> Ct r o a -> Ct r o a
x +# c = (x +) <$> c

(-#) :: Num a => a -> Ct r o a -> Ct r o a
x -# c = (`subtract` x) <$> c

(*#) :: Num a => a -> Ct r o a -> Ct r o a
x *# c = (x *) <$> c

(#+) :: Num a => Ct r o a -> a -> Ct r o a
c #+ x = (+ x) <$> c

(#-) :: Num a => Ct r o a -> a -> Ct r o a
c #- x = (x `subtract`) <$> c

(#*) :: Num a => Ct r o a -> a -> Ct r o a
c #* x = (* x) <$> c

(++#) :: [a] -> Ct r o [a] -> Ct r o [a]
x ++# c = (x ++) <$> c

(#++) :: Ct r o [a] -> [a] -> Ct r o [a]
c #++ x = (++ x) <$> c

(.#) :: (a -> b) -> Ct r o a -> Ct r o b
(.#) = (<$>)

(#.) :: Ct r o a -> (a -> b) -> Ct r o b
(#.) = flip (<$>)

infixr 4 .#

infixl 4 #.

(===) :: (Show a, Eq a) => a -> a -> IO ()
x === y = shouldBe x y

(.==) :: (Show a, Eq a) => Ct a o o -> a -> IO ()
x .== y = shouldBe (eval x) y

infix 1 ===, .==

ifelse :: a -> a -> Bool -> a
ifelse x y c = if c then x else y

ioEqual :: IO () -> String -> IO ()
ioEqual io expected = captureOutput io >>= (=== expected) . fst

listRegularFiles :: FilePath -> IO [FilePath]
listRegularFiles f = x f
  where
    x =
      listDirectory
        >=> traverse (\p -> (f </> p,) <$> getFileStatus (f </> p))
        >=> return . (fst <$>) . filter (isRegularFile . snd)
        >=> (return . sort)

spec :: Spec
spec = describe "" $ do
  it "computes" $ do
    let x =
          ( (4 *# (3 +# 2 -# retT @Int 1))
              #- 2
              #+ 3
          )
            #* 4
        y = ("hello" ++) .# (retT "World" #. (++ "!"))
    x .== 68
    y .== "helloWorld!"

  it "loops" $ do
    let x = 4 *# (3 +# 2 -# shift (\k -> retT (k (k $ i 1))))
    eval x === (-44)

  it "resets" $ do
    let x = resetT $ 4 *# (3 +# 2 -# shift exitT)
        captured = eval @(Int -> Int) x

    captured 1 === 16
    captured 2 === 12

  describe "2.3" $ do
    it "Exercise 2" $ do
      5
        *# resetT (retT (2 * 3) #+ 3 * 4)
        .== i 90

      resetT
        ( retT (i 2 == 3)
            #. ifelse "hello" "hi"
        )
        #++ "world"
        .== "hiworld"

      fst
        .# resetT (retT (1 + 2) #. (\x -> (x, x)))
        .== i 3

      length
        .# resetT ("x" ++# show .# retT (3 + i 1))
        .== 2

  describe "2.6" $ do
    it "Exercise 5" $ do
      let k1 =
            resetT (5 *# (shift exitT #+ 3 * 4))
              & eval
          k2 =
            resetT (shift exitT #. ifelse "hello" "hi" #++ "!")
              & eval
          k3 = resetT (fst .# (shift exitT #. (\x -> (x, x)))) & eval

      k1 1 === i 65
      k2 True === "hello!"
      k2 False === "hi!"
      k3 1 === i 1

  describe "2.7" $ do
    it "walks" $ do
      let t0 = Empty @Int
          t1 = Node @Int Empty 1 Empty
          t2 = Node (Node (Node Empty 1 Empty) 2 Empty) 3 Empty
          t3 = Node t1 10 t2

      gprint (walkTree t0) `ioEqual` "Done\n"
      gprint (walkTree t2) `ioEqual` "value:1\nvalue:2\nvalue:3\nDone\n"
      gprint (walkTree t3) `ioEqual` "value:1\nvalue:10\nvalue:1\nvalue:2\nvalue:3\nDone\n"
      coerce @(Sum Int) @Int (foldMap Sum (walkTree t0)) === 0
      coerce @(Sum Int) @Int (foldMap Sum (walkTree t1)) === 1
      coerce @(Sum Int) @Int (foldMap Sum (walkTree t2)) === 6
      coerce @(Sum Int) @Int (foldMap Sum (walkTree t3)) === 17

    it "Exercise 7" $ do
      let t1 = Node @Int (Node (Node Empty 1 Empty) 2 Empty) 3 Empty
          t2 = Node Empty 1 (Node (Node Empty 2 Empty) 3 Empty)
          t3 = Node Empty 1 (Node Empty 2 (Node Empty 3 Empty))
          s0 = Empty
          s1 = Node @Int (Node (Node Empty 2 Empty) 3 Empty) 1 Empty
          s2 = Node @Int (Node (Node Empty 1 Empty) 2 Empty) 1 Empty
          s3 = Node Empty 0 t1
          s4 = Node t1 0 Empty

      sameFringeG t1 t1 === True
      sameFringeG t1 t2 === True
      sameFringeG t1 t3 === True

      traverse_
        ((=== False) . sameFringeG t1)
        [ s0,
          s1,
          s2,
          s3,
          s4
        ]

  describe "2.8" $ do
    it "Exercise 8" $ do
      let printMsg :: String -> String
          printMsg =
            eval $
              ("hello " ++# shift exitT)
                #++ "!"
          prinTValue :: Show a => a -> String
          prinTValue =
            eval $
              ("value: " ++# (show .# shift exitT)) #++ " !"

      printMsg "world" === "hello world!"
      prinTValue @Int 10 === "value: 10 !"

  it "test" $ do
    let proc :: Ct (Int -> () -> String) (String -> String) Int
        proc =
          retT 2
            #>>= ( \x ->
                     shift exitT
                       #>>= ( \y ->
                                retT (x * y)
                                  #>>= ( \z ->
                                           shift (\k -> exitT (\() -> k (z + 1) "hello"))
                                             #>>= ( \u -> retT $ z * u
                                                  )
                                       )
                            )
                 )
        kproc = runCt proc (\x msg -> msg <> "\nvalue:" <> show x)

    kproc 1 () === "hello\nvalue:6" -- (2 * 1 + 1) * (2 * 1)
    kproc 10 () === "hello\nvalue:420" -- (2 * 10 + 1) * (2 * 10)
  describe "2.10: State" $ do
    let runIntState :: MonadIO m => CtTState Int m Int -> m Int
        runIntState (CtT c) = c (return . (const . return)) >>= ($ 0)

    it "state" $ do
      let k1 = do
            tick
            tick
            tick
            get
          k2 = do
            a <- k1
            tick
            tick
            (a *) <$> get
          k4 = k2 >> k2

      runIntState k1 >>= (=== 3)
      runIntState k2 >>= (=== 15)
      runIntState k4 >>= (=== 80)

    it "Exercise 9" $ do
      let t = do
            tick
            a <- get
            tick
            (`subtract` a) <$> get

      runIntState t >>= (=== (-1))

    it "Exercise 10" $ do
      runIntState
        ( do
            put 2
            modify (* 10)
            get
        )
        >>= (=== 20)

  describe "2.12" $ do
    let eitherCt :: (Semigroup m) => (a, a) -> Ct m m a
        eitherCt (a, b) = ct $ \k -> k a <> k b

        choiceCt :: (Monoid m) => [a] -> Ct m m a
        choiceCt l = ct $ \k -> foldMap k l

    it "either" $ do
      let printPair :: (MonadIO m, Semigroup (m ()), Show a) => (a, a) -> Ct (m ()) (m ()) (m ())
          printPair p = do
            x <- eitherCt p
            return $ liftIO $ putStrLn $ "value:" <> show x

      eval (printPair (1 :: Int, 2)) `ioEqual` "value:1\nvalue:2\n"
      eval (printPair (3 :: Int, 4)) `ioEqual` "value:3\nvalue:4\n"

      eval
        ( do
            x <- eitherCt (True, False)
            y <- eitherCt (True, False)
            return $
              liftIO $
                when ((x || y) && (x || not y) && (not x || not y)) $
                  putStrLn $
                    show x <> "," <> show y
        )
        `ioEqual` "True,False\n"

    it "Exercise 13" $ do
      eval
        ( do
            x <- choiceCt [1 .. 5 :: Int]
            y <- choiceCt [1 .. 5]
            z <- choiceCt [1 .. 5]
            if x * x + y * y == z * z
              then return [(x, y, z)]
              else return []
        )
        === [(3, 4, 5), (4, 3, 5)]

  describe "Cont monad" $ do
    it "state with cont" $ do
      let run :: Monad m => CS.ContState Int m Int -> m Int
          run c = do
            f <- CS.runContT c $ \a -> return (const $ return a)
            f 0

      run
        ( do
            CS.tick
            CS.tick
            CS.tick
            CS.get
        )
        >>= (=== 3)
      run
        ( do
            CS.put 1
            CS.modify (* 10)
            CS.get
        )
        >>= (=== 10)

    it "writer with Cont" $ do
      let run :: (Monoid w, Monad m) => CS.ContWrite w m () -> m w
          run c = CS.runContT c (const (return mempty))

      run
        ( do
            CS.tell "Hello"
            CS.tell " "
            CS.tell "World"
        )
        >>= (=== "Hello World")
      run
        ( do
            CS.tell [1, 2, 3 :: Int]
            CS.tell [4, 5, 6]
        )
        >>= (=== [1, 2, 3, 4, 5, 6])

  describe "Misc" $ do
    let -- From "Why would you use ContT": https://ro-che.info/articles/2019-06-07-why-use-contt

    it "reads multiple files" $ do
      let t :: CtT () () IO ()
          t = do
            f <- liftIO $ listRegularFiles "./test/delimitedContinuation/"
            handles <- traverse (\p -> CtT $ withFile p ReadMode) f
            liftIO $ readFirstLine handles

          readFirstLine :: [Handle] -> IO ()
          readFirstLine = traverse_ (hGetLine >=> putStrLn)

      evalT t
      evalT t `ioEqual` "module ContState\n\
        \{-# LANGUAGE FlexibleInstances #-}\n\
        \{-# LANGUAGE FlexibleInstances #-}\n\
        \{-# LANGUAGE DerivingVia #-}\n\
        \{-# LANGUAGE FlexibleInstances #-}\n\
        \{-# LANGUAGE GADTs #-}\n\
        \import Test.Hspec\n"
