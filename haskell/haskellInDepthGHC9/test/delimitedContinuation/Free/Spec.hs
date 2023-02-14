{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

module Free.Spec
  ( spec,
  )
where

import Control.Monad.Trans.Cont
import Free.Ct
import Data.Foldable
import Data.Function
import System.IO.Extra
import Test.Hspec
import Free.TreeWalk

testCont :: (Show r, Eq r) => Cont r r -> r -> IO ()
testCont c expected = evalCont c `shouldBe` expected

-- extract the capture (a -> b)
extractCapture :: a -> Ctf (a -> r) r r -> a -> r
extractCapture a = evalCtfWith const ($ a)

spec :: Spec
spec = describe "Delimited continuations tutorial\nhttp://pllab.is.ocha.ac.jp/~asai/cw2011tutorial/main-e.pdf" $ do
  it "Suspended" $ do
    let x :: Ctf (Int -> Int) (Int -> Int) (Int -> Int)
        x = resetCtfWith const ($ 1) $ do
          y <- liftct id
          return $ 10 * y
        r = evalCtf x
    r 1 `shouldBe` 10
    r 2 `shouldBe` 20

    let x' :: Ctf (Int -> Int) (Int -> Int) (Int -> Int)
        x' = resetCtf $ do
          y <- liftct $ \k -> ($ 1) . k
          return $ const $ 10 * y
        r' = evalCtf x'
    r' 2 `shouldBe` 20

  describe "2.5" $ do
    it "Exercise 3: discard continuation" $ do
      ( do
          y <-
            reset
              ( do
                  x <- shift $ \_ -> return (-1)
                  return $ x + 3 * 4
              )
          return $ y * 5
        )
        `testCont` (-5 :: Int)

      ( do
          y <-
            reset
              ( do
                  x <- shift $ \_ -> return "OOPS"
                  return $ if x == 2 then "Hello" else "hi"
              )
          return $ y ++ "World"
        )
        `testCont` "OOPSWorld"

      ( do
          y <-
            reset
              ( do
                  x <- shift $ \_ -> return ""
                  return $ "x" ++ x
              )
          return $ length y
        )
        `testCont` 0

  describe "2.7" $ do
    it "Excercise 5: extract continuation" $ do
      let f1 =
            ( do
                x <- liftct id
                return $ 5 * (x + 12)
            )
              & extractCapture 1
      f1 1 `shouldBe` 65

      let f2 =
            ( do
                x <- liftct id
                let prefix = if x then "hello" else "hi"
                return $ prefix ++ "World"
            )
              & extractCapture True
      f2 True `shouldBe` "helloWorld"
      f2 False `shouldBe` "hiWorld"

      let f3 =
            ( do
                x <- liftct id
                return (x, x)
            )
              & extractCapture 0
      f3 1 `shouldBe` (1, 1)
      f3 2 `shouldBe` (2, 2)

    it "Exercise 6: extract continuation" $ do
      let idCtf :: [Int] -> Ctf ([Int] -> [Int]) [Int] [Int]
          idCtf [] = liftct id
          idCtf (x : xs) = do
            xs' <- idCtf xs
            return $ (2 * x) : xs'
          id' = extractCapture [] . idCtf

      id' [1, 2, 3] [4, 5] `shouldBe` [2, 4, 6, 4, 5]
      id' [1, 2, 3] [] `shouldBe` [2, 4, 6]
      id' [] [4, 5] `shouldBe` [4, 5]

  describe "2.7" $ do
    it "walks a tree" $ do
      let gen = walkTree tree4
      (s, ()) <- captureOutput $ printGen gen
      s `shouldBe` "value:1\nvalue:4\nvalue:2\nvalue:5\nvalue:3\nvalue:6\nvalue:1\n"
      sum gen `shouldBe` 22
      collectGen gen `shouldBe` [1,4,2,5,3,6,1]

    it "Exercise 7" $ do 
      let s0 = Node (Node (Node Empty 1 Empty) 2 Empty) 3 Empty
          s1 = Node Empty 1 (Node Empty 2 (Node Empty 3 Empty))
          s2 = Node Empty 1 (Node (Node Empty 2 Empty) 3 Empty)
          s3 = Node Empty 1 s0

      sameFringe s0 s0 `shouldBe` True
      sameFringe s0 s1 `shouldBe` True
      sameFringe s1 s0 `shouldBe` True
      sameFringe s1 s2 `shouldBe` True
      sameFringe s2 s1 `shouldBe` True

      sameFringe s0 s3 `shouldBe` False
      sameFringe s3 s0 `shouldBe` False
      sameFringe s0 Empty `shouldBe` False
      sameFringe Empty s0 `shouldBe` False

  describe "2.8" $ do
    it "Exercise 8" $ do
      let f :: Ctf (String -> String) String String
          f = do
              x <- liftct $ \k -> k . ("Hello" <>)
              return $ x <> "!"
          printMessage = extractCapture "" f
      printMessage "WORLD" `shouldBe` "HelloWORLD!"
      printMessage " WORLD " `shouldBe` "Hello WORLD !"

  it "Duplication" $ do
    let either' :: Monad m => a -> a -> Cont (m r) a
        either' x y = shift $ \k -> return $ k x >> k y

    (msg, ()) <- captureOutput $ evalCont $ reset $ do
      x <- either' 1 2
      return $ putStrLn $ "value:" <> show x
    msg `shouldBe` "value:1\nvalue:2\n"

  it "Exercise 12: choice" $ do
    let choice :: Monad m => [a] -> Cont (m ()) a
        choice l = shift $ \k -> return $ traverse_ k l

    (msg, ()) <- captureOutput $ evalCont $ reset $ do
      x <- choice [1 .. 5]
      return $ putStrLn $ "value:" <> show x
    msg `shouldBe` "value:1\nvalue:2\nvalue:3\nvalue:4\nvalue:5\n"
