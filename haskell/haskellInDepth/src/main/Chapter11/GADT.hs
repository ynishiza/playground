{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

module Chapter11.GADT (run) where

import Data.Char
import Data.Foldable
import GHC.Exts
import Utils

run :: TestState
run =
  createChapterTest
    "11.4.1"
    "Simple GADT"
    ( do
        let n0 = N 0
            n1 = N 1
            s0 = S ""
            shello = S "Hello"
            ca = C 'a'
            c0 = C '\NUL'
            nonEmptyList =
              [ DynWrapped n1,
                DynWrapped ca,
                DynWrapped shello
              ]
            emptyList =
              [ DynWrapped n0,
                DynWrapped c0,
                DynWrapped s0
              ]
         in do
              assertIsEqual (getDynValue n0) 0
              assertIsEqual (getDynValue c0) '\NUL'
              assertIsEqual (getDynValue s0) ""

              printBanner "Test emptiness"
              assertIsEqual (isEmpty n0) True
              assertIsEqual (isEmpty n1) False
              assertIsEqual (isEmpty s0) True
              assertIsEqual (isEmpty shello) False
              assertIsEqual (isEmpty c0) True
              assertIsEqual (isEmpty ca) False
              traverse_ (flip assertIsEqual True . isEmptyWrapped) emptyList
              traverse_ (flip assertIsEqual False . isEmptyWrapped) nonEmptyList
        testDone
    )

data Dyn a where
  S :: IsString s => {getS :: s} -> Dyn s
  C :: {getC :: Char} -> Dyn Char
  N :: Num a => {getN :: a} -> Dyn a

getDynValue :: Dyn a -> a
getDynValue v@(S _) = getS v
getDynValue v@(C _) = getC v
getDynValue v@(N _) = getN v

isEmpty :: Eq a => Dyn a -> Bool
isEmpty (S v) = v == fromString ""
isEmpty (C c) = ord c == 0
isEmpty (N v) = v == 0

data DynWrapped where
  DynWrapped :: Eq a => Dyn a -> DynWrapped

isEmptyWrapped :: DynWrapped -> Bool
isEmptyWrapped (DynWrapped v) = isEmpty v
