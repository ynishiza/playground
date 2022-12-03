{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoStarIsType #-}
{-# LANGUAGE OverloadedStrings #-}

module Chapter11.TypeOperators
  ( run,
    (+),
  )
where

import Fmt
import Utils

run :: TestState
run =
  createChapterTest
    "11.1.3"
    "TypeOperators"
    ( do
        let x = 1 :*: 2 :*: 3 :: Int * Int * Int
            y = Inl (Inr 2) :: (Char + Int) + Char            
            -- z = Inr (Inr 1) :: a + Int + b
            -- z1 = Inl (Inl 1) :: Int + a + b
            -- z12 = Inl (Inl 1) :: (Int + a) + b
            -- z2 = Inr (Inl 1) :: ant + Int + b
            -- z3 = Inr 1 :: c + d + Int
         in do
              print x
              print y

        let 
          -- p1 = Inl $ Inl 1 :: Point Int
          p1 = Inl $ Inl 1 :: Int + Int + (Int * Int)
          p2 = Inl $ Inr $ 1 :*: 2 :: Point Int
          p3 = Inr $ 1 :*: 2 :*: 3 :: Point Int
          in do
            fmtLn $ nameF "p1" (build $ show p1)
              <> nameF "p2" (build $ show p2)
              <> nameF "p3" (build $ show p3)
    )

data a + b = Inl !a | Inr !b deriving (Show)

data a * b = !a :*: !b deriving (Show)

type Point a = a + a * a + a * a * a
-- type Point a = (a + (a * a)) + ((a * a) * a)

infixl 6 +
infixl 7 *

