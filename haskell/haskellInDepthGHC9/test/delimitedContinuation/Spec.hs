{-# LANGUAGE DerivingVia #-}

module Spec
  ( spec,
  )
where

import ContState qualified as CS
import Control.Monad
import Control.Monad.IO.Class
import Ct
import Data.Coerce
import Data.Foldable
import Data.Function
import Data.Monoid
import State
import System.IO.Extra
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

spec :: Spec
spec = describe "" $ do
  it "computes" $ do
    let x =
          ( (4 *# (3 +# 2 -# ret @Int 1))
              #- 2
              #+ 3
          )
            #* 4
        y = ("hello" ++) .# (ret "World" #. (++ "!"))
    x .== 68
    y .== "helloWorld!"

  it "loops" $ do
    let x = 4 *# (3 +# 2 -# shift (\k -> ret (k (k $ i 1))))
    eval x === (-44)

  it "resets" $ do
    let x = reset $ 4 *# (3 +# 2 -# shift exit)
        captured = eval @(Int -> Int) x

    captured 1 === 16
    captured 2 === 12

  describe "2.3" $ do
    it "Exercise 2" $ do
      5
        *# reset (ret (2 * 3) #+ 3 * 4)
        .== i 90

      reset
        ( ret (i 2 == 3)
            #. ifelse "hello" "hi"
        )
        #++ "world"
        .== "hiworld"

      fst
        .# reset (ret (1 + 2) #. (\x -> (x, x)))
        .== i 3

      length
        .# reset ("x" ++# show .# ret (3 + i 1))
        .== 2

  describe "2.6" $ do
    it "Exercise 5" $ do
      let k1 =
            reset (5 *# (shift exit #+ 3 * 4))
              & eval
          k2 =
            reset (shift exit #. ifelse "hello" "hi" #++ "!")
              & eval
          k3 = reset (fst .# (shift exit #. (\x -> (x, x)))) & eval

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
              ("hello " ++# shift exit)
                #++ "!"
          prinTValue :: Show a => a -> String
          prinTValue =
            eval $
              ("value: " ++# (show .# shift exit)) #++ " !"

      printMsg "world" === "hello world!"
      prinTValue @Int 10 === "value: 10 !"

  it "test" $ do
    let proc :: Ct (Int -> () -> String) (String -> String) Int
        proc =
          ret 2
            #>>= ( \x ->
                     shift exit
                       #>>= ( \y ->
                                ret (x * y)
                                  #>>= ( \z ->
                                           shift (\k -> exit (\() -> k (z + 1) "hello"))
                                             #>>= ( \u -> ret $ z * u
                                                  )
                                       )
                            )
                 )
        kproc = runCt proc (\x msg -> msg <> "\nvalue:" <> show x)

    kproc 1 () === "hello\nvalue:6" -- (2 * 1 + 1) * (2 * 1)
    kproc 10 () === "hello\nvalue:420" -- (2 * 10 + 1) * (2 * 10)

  describe "2.10: State" $ do
    let runIntState :: CtState Int Int -> Int
        runIntState (Ct c) = c const 0

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

      runIntState k1 === 3
      runIntState k2 === 15
      runIntState k4 === 80

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

    it "Exercise 12" $ do
      let t = do
            tick
            a <- get
            tick
            (`subtract` a) <$> get

      runIntState t === (-1)

    it "Exercise 13" $ do
      runIntState
        ( do
            put 2
            modify (* 10)
            get
        )
        === 20

  describe "2.12" $ do
    let eitherCt :: (Semigroup m) => (a, a) -> Ct m m a
        eitherCt (a, b) = Ct $ \k -> k a <> k b

        choiceCt :: (Monoid m) => [a] -> Ct m m a
        choiceCt l = Ct $ \k -> foldMap k l

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
