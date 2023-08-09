#!/usr/bin/env stack
{-
  Run with
    stack exec -- src/scratch/<name>.hs
    stack ghci -- src/scratch/<name>.hs
-}

{-
  Based on: https://www.well-typed.com/blog/2019/05/integrated-shrinking/
-}
main :: IO ()
main = putStrLn "Hello"
