{-# LANGUAGE OverloadedStrings #-}

module Prime (isPrime1, isPrime, primeList, primeListV2, isPrimeV2) where

import Utils

primeList :: [Integer]
primeList = primeListV1

isPrime :: Integer -> Bool
isPrime = isPrimeV1

primeListV1 :: [Integer]
primeListV1 = 2 : 3 : filter isPrimeV1 [5 ..]

primeListV2 :: [Integer]
primeListV2 = f [2, 3 ..]
  where
    f (x : xs) = x : f [v | v <- xs, v `mod` x /= 0]
    f [] = shouldNeverHappen

isPrimeV2 :: Integer -> Bool
isPrimeV2 n
  | n <= 0 = shouldNeverHappen
  | otherwise = (n ==) $ last $ (takeWhile (n >=)) $ primeListV2

isPrimeV1 :: Integer -> Bool
isPrimeV1 n
  | n <= 0 = shouldNeverHappen
  | otherwise =
    not
      ( any
          (isDivisible n)
          ( takeWhile ((n >=) . square) primeList
          )
      )

square :: Num a => a -> a
square n = n * n

isDivisible :: Integer -> Integer -> Bool
isDivisible n m = n `mod` m == 0

isPrime1 :: Integer -> Bool
isPrime1 n = all (\x -> n `mod` x /= 0) [2 .. (n -1)]
