{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

module Chapter11.GADT2
  ( run,
  )
where

import Utils

run :: TestState
run =
  createChapterTest
    "11.4.2"
    "GADT"
    ( do
        let n1 = NumLit 1
            n0 = NumLit 0
            n2 = NumLit 2
            bf = BoolLit False
            bt = BoolLit True
            e1 = Add n2 n1
            e2 = Add e1 e1
            e3 = Add e2 e1
         in do
              assertIsEqual (parse e1) 3
              assertIsEqual (parse e2) 6
              assertIsEqual (parse e3) 9
              assertIsEqual (parse $ IsZero n0) True
              assertIsEqual (parse $ IsZero n1) False
              assertIsEqual (parse $ IsZero e3) False

        testDone
    )

data Expr a where
  NumLit :: Num a => a -> Expr a
  BoolLit :: Bool -> Expr Bool
  Add :: Num a => Expr a -> Expr a -> Expr a
  Mult :: Num a => Expr a -> Expr a -> Expr a
  IsZero :: (Num a, Eq a) => Expr a -> Expr Bool
  If :: Expr Bool -> Expr a -> Expr a -> Expr a

parse :: Expr a -> a
parse (NumLit v) = v
parse (BoolLit v) = v
parse (Add e1 e2) = parse e1 + parse e2
parse (Mult e1 e2) = parse e1 * parse e2
parse (IsZero e) = parse e == 0
parse (If p e1 e2) = if parse p then parse e1 else parse e2
