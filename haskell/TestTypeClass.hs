{-# LANGUAGE FunctionalDependencies #-}
module TestTypeClass
  ( testTypeClass,
  testMultiParameters,
  )
where

import Data.Int
import GHC.Types
import TestUtils

class MyDoSomething c where
  doSomething :: c -> c
  printSomething :: Show c => c -> IO ()
  printSomething x = putStrLn $ "x=" ++ show x ++ " MyDoSomething=" ++ show (doSomething x)

instance MyDoSomething Int where
  doSomething x = 2 * x

instance MyDoSomething Double where
  doSomething x = 3 * x

class MyDoSomethingMult a b where
  doSomethingMult :: a -> b -> a
  printSomethingMult :: (Show a, Show b) => a -> b -> IO ()
  printSomethingMult x y =
    putStrLn $
      "x="
        ++ show x
        ++ " "
        ++ "y="
        ++ show y
        ++ " "
        ++ "MyDoSomethingMult="
        ++ show (doSomethingMult x y)

instance MyDoSomethingMult String Char where
  doSomethingMult x y = x ++ replicate 10 y

instance MyDoSomethingMult Char String where
  doSomethingMult x y | v : _ <- y = v | otherwise = x

instance MyDoSomethingMult Char Char where
  doSomethingMult x y | x > y = x | otherwise = y

instance MyDoSomethingMult Int Double where
  doSomethingMult x y = x + round y

instance MyDoSomethingMult Double Int where
  doSomethingMult x y = 2 * fromIntegral y * x

instance MyDoSomethingMult Double Double where
  doSomethingMult x y = x * y

testTypeClass =
  callTest
    ( do
        printSomething (1 :: Int)
        printSomething (1.0 :: Double)

        printSomethingMult "Hello" 'y'
        printSomethingMult 'y' "Hello"
        printSomethingMult 'y' ""
        printSomethingMult 'a' 'b'
        printSomethingMult 'x' 'b'
        printSomethingMult (2 :: Int) (3 :: Double)
        printSomethingMult (2 :: Double) (3 :: Int)
        printSomethingMult (2 :: Double) (3 :: Double)
        testDone
    )
    ""

class MultiParameterTest a b where 
  (#+) :: a -> b -> a
instance MultiParameterTest Int Int where
  (#+) x y = x + y 
instance MultiParameterTest Int Double where
  (#+) x y = x * round y 
instance MultiParameterTest a (a -> a) where
  (#+) x f = f x

-- with dependency
class MultiParameterTest2 a b | a -> b where 
  (#++) :: a -> b -> a
instance MultiParameterTest2 Int Int where
  (#++) x y = x + y 
testMultiParameters = callTest (do
  let
    x :: Int
    -- x = (1::Int) #+ (1::Int)
    x = (1::Int)#+ (1::Int)
    s = (1::Int) #+ (1::Double)
    y = (1::Int)  #+((*2)::Int -> Int)

    x0 = (1::Int) #++ 1    
  print x
  print y
  print s
  testDone) "testMultiParameters"

-- Functional dependencies 
--
class MyFunctionalDependenciesTest a b c d | a -> b, c -> d where        
instance MyFunctionalDependenciesTest Int Int Int Int where       
instance MyFunctionalDependenciesTest Double Int Int Int where   
instance MyFunctionalDependenciesTest Int Int Double Int where  
-- instance MyFunctionalDependenciesTest Int Double Double Int where   -- FAIL
--

class MyClass c where                     
  fn :: c -> Bool
-- instance MyClass Bool where
-- instance MyClass Maybe where

class MyClass2 c where
  fn2 :: c a -> a
