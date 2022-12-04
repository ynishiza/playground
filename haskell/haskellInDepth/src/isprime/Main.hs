module Main (main) where

import System.TimeIt
import System.Environment

main :: IO ()
main = getArgs >>= timeIt . print . isPrime . read . head

isPrime :: Integer -> Bool
isPrime n  = all (\x -> n `mod` x /= 0) [2..(n-1)] 
