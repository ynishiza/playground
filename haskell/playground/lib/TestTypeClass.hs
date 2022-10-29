{-# LANGUAGE FunctionalDependencies #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant $" #-}
{-# HLINT ignore "Redundant bracket" #-}

module TestTypeClass
  ( testTypeClass,
    testMultiParameters,
    testDerivedInstance,
    runAll,
  )
where

import Data.Int
import GHC.Types
import TestUtils

runAll =
  callTest
    ( do
        testDerivedInstance
        testTypeClass
        testMultiParameters
    )
    "TestTypeClass"

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
    "testTypeClass"

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

testMultiParameters =
  callTest
    ( do
        let x :: Int
            -- x = (1::Int) #+ (1::Int)
            x = (1 :: Int) #+ (1 :: Int)
            s = (1 :: Int) #+ (1 :: Double)
            y = (1 :: Int) #+ ((* 2) :: Int -> Int)

            x0 = (1 :: Int) #++ 1
        print x
        print y
        print s
        testDone
    )
    "testMultiParameters"

-- Functional dependencies
--
class MyFunctionalDependenciesTest a b c d | a -> b, c -> d

instance MyFunctionalDependenciesTest Int Int Int Int

instance MyFunctionalDependenciesTest Double Int Int Int

instance MyFunctionalDependenciesTest Int Int Double Int

-- instance MyFunctionalDependenciesTest Int Double Double Int where   -- FAIL
--

class MyNullaryClass (c :: Type)

class MyUnaryClass (c :: Type -> Type)

class MyBinaryClass (c :: Type -> Type -> Type)

instance MyUnaryClass []

instance MyNullaryClass [Int]

instance MyNullaryClass ([] Bool)

instance MyBinaryClass (->)

instance MyUnaryClass ((->) Int)

instance MyUnaryClass ((->) a)

instance MyNullaryClass ((->) Int Int)

instance MyNullaryClass ((->) a b)

instance MyBinaryClass (,)

instance MyUnaryClass ((,) Int)

instance MyUnaryClass ((,) a)

instance MyNullaryClass ((,) a b)

class MyConflict c where
  check :: c -> Bool

-- instance Real a => MyConflict a where
instance MyConflict Int where
  check x = x > 0

instance MyConflict Double where
  check x = x > 0

instance Num a => MyConflict [a] where
  check (x : _) = True

-- instance Read a => MyConflict [a] where
--   check (x:_) = False
instance MyConflict [Char] where
  check (x : _) = False

instance MyConflict [Int] where check (x : _) = False

class ClassOverlap c where
  isValid :: c -> Bool

instance ClassOverlap [Int] where
  isValid _ = False

instance Num c => ClassOverlap [c] where
  isValid _ = True

-- x = isValid [0::Int]     -- ERROR. Overlapping instance
--

-- Derived
data MyDerivedPair a b = MyDerivedPair a b deriving (Eq, Ord, Bounded, Read, Show)
data MyDerivedEnum = DA1 | DA2 | DA3 | DA4 | DA5 | DA6 deriving (Eq, Ord, Bounded, Enum, Read, Show)

testDerivedInstance =
  callTest
    ( do
        printBanner "Eq,Ord"
        print $ MyDerivedPair 1 2 < MyDerivedPair 1 3
        print $ MyDerivedPair 2 2 < MyDerivedPair 1 3
        print $ MyDerivedPair 1 1 == MyDerivedPair 1 1
        print $ MyDerivedPair 1 'a' == MyDerivedPair 1 'a'
        -- print $ Pair 1 (*) == Pair 1 (*)   NO

        printBanner "Bounded"
        print $ (minBound :: MyDerivedPair Int Int)
        print $ (maxBound :: MyDerivedPair Int Int)
        print $ (minBound :: MyDerivedPair Int Char)
        print $ MyDerivedPair (minBound::Int) (minBound::Char)
        print $ (maxBound :: MyDerivedPair Int Char)
        -- print $ (maxBound::Pair Int Double)    NO
        print $ (minBound:: MyDerivedEnum)
        print $ (maxBound:: MyDerivedEnum)

        print $ DA1
        print $ [(DA1)..]
        print $ [(DA1),(DA3)..]
        -- print [DA1..]

        printBanner "Show"
        print (MyDerivedPair 1 2)
        testDone
    )
    "testDerivedInstance"
