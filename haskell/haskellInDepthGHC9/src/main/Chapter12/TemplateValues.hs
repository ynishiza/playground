{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExistentialQuantification #-} 
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Unused LANGUAGE pragma" #-}
module Chapter12.TemplateValues (
  run
  ) where

import Data.Foldable
import Fmt
import Utils

run :: TestState
run = createChapterTest "12.3" "Template Haskell terms" (do
  printBannerWrap "Expr" $
    traverse_ printContainer [
      IOContainer "one" [| 1 |],
      IOContainer "+1" [| (+1) |]
                            ]
  printBannerWrap "Dec" $
    traverse_ printContainer [
    IOContainer "data MyType = A | B" [d| data MyType = A | B |],
    IOContainer "data MyType = A deriving (Show, Eq)" [d| data MyType = A deriving (Show, Eq) |],
    IOContainer "newtype MyType a = A a" [d| newtype MyType a b = A a |],
    IOContainer "type Size = Int" [d| type Size = Int |]
                             ]
  printBannerWrap "Pattern" $
    traverse_ printContainer [
    IOContainer "_" [p| _ |],
    IOContainer "x" [p| x |],
    IOContainer "Just x" [p| Just x |],
    IOContainer "(x,y)" [p| (x,y) |],
    IOContainer "x:xs" [p| x:xs |],
    IOContainer "(1,y)" [p| (1,y) |],
    IOContainer "(1,y,z)" [p| (1,y,z) |],
    IOContainer "v@(_,_)" [p| v@(_,_) |],
    IOContainer "Just (Just (Jst x))" [p| Just (Just (Just x)) |]
                             ]


  printBannerWrap "Type" $ 
    traverse_ printContainer [
      IOContainer "()" [t| () |],
      IOContainer "Int" [t| Int |],
      IOContainer "Maybe " [t| Maybe |],
      IOContainer "Maybe Int" [t| Maybe Int |],
      IOContainer "[]" [t| [] |],
      IOContainer "[Int]" [t| [Int] |],
      IOContainer "(,)" [t| (,) |],
      IOContainer "(Int,Double)" [t| (Int, Double) |],
      IOContainer "(->)" [t| (->) |],
      IOContainer "Int -> Double" [t| Int -> Double |],
      IOContainer "forall a. a" [t| forall a. a |],
      IOContainer "forall a. (Show a, Eq a)" [t| forall a. (Show a, Eq a) |]
                             ]

  testDone)

data IOContainer = forall a. Show a => IOContainer String (IO a)

printContainer :: IOContainer -> IO ()
printContainer (IOContainer n c) = do
  v <- c
  fmtLn $ n ||+":"+||v ||+""
