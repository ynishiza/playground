{-# LANGUAGE OverloadedStrings #-}

module Prime (isPrime1, isPrime, primeList) where

primeList :: [Integer]
primeList = 2 : 3 : filter isPrime [5 ..]

isPrime :: Integer -> Bool
isPrime n =
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
