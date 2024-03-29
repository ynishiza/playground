module Radar
  ( allDirections,
    allTurns,
    Direction (..),
    Turn (..),
    rotate,
    rotateMany,
    orient,
    orientMany,
    rotateManyInSteps,
    getEnumLength,
    module X
  )
where

import Data.Proxy as X
import Fmt

class (Bounded a, Enum a, Eq a) => CyclicEnum a where
  csucc :: a -> a
  csucc x
    | x == maxBound = minBound
    | otherwise = succ x

  cpred :: a -> a
  cpred x
    | x == minBound = maxBound
    | otherwise = pred x

data Direction = North | East | South | West deriving (Eq, Ord, Read, Show, Enum, Bounded)

instance CyclicEnum Direction

instance Num Direction where
  fromInteger = toEnum . fromInteger
  abs = enumApplyUnaryOp abs
  negate = enumApplyUnaryOp negate
  signum = enumApplyUnaryOp signum
  (+) = enumApplyBinaryOp (+)
  (*) = enumApplyBinaryOp (*)

data Turn = TNone | TRight | TAround | TLeft deriving (Eq, Ord, Show, Read, Enum, Bounded)

instance Semigroup Turn where
  t1 <> t2 = toEnum $ fromEnum $ t1 + t2

instance Monoid Turn where
  mempty = TNone

instance CyclicEnum Turn

instance Num Turn where
  fromInteger = toEnum . fromInteger
  abs = enumApplyUnaryOp abs
  negate = enumApplyUnaryOp negate
  signum = enumApplyUnaryOp signum
  (+) = enumApplyBinaryOp (+)
  (*) = enumApplyBinaryOp (*)

getEnumLength :: forall a. (Enum a, Bounded a) => Proxy a -> Int
getEnumLength _ = fromEnum (maxBound @a) - fromEnum (minBound @a) + 1

enumApplyUnaryOp :: forall a. (Bounded a, Enum a) => (Int -> Int) -> a -> a
enumApplyUnaryOp op x = toEnum $ mod (op $ fromEnum x) (getEnumLength (Proxy @a))

enumApplyBinaryOp :: forall a. (Bounded a, Enum a) => (Int -> Int -> Int) -> a -> a -> a
enumApplyBinaryOp op x y = toEnum $ mod (fromEnum x `op` fromEnum y) (getEnumLength (Proxy @a))

-- f :: forall a .a -> a
-- f x = (x::a)

rotate :: Turn -> Direction -> Direction
rotate t d = case t of
  TRight -> csucc d
  TAround -> csucc $ csucc d
  TLeft -> cpred d
  _ -> d

rotateMany :: Direction -> [Turn] -> Direction
rotateMany = foldr rotate

rotateManyInSteps :: Direction -> [Turn] -> [Direction]
rotateManyInSteps = scanl (flip rotate)

orientMany :: [Direction] -> [Turn]
orientMany [] = []
orientMany [_] = []
orientMany (x : y : rest) = orient x y : orientMany (y : rest)

orient :: Direction -> Direction -> Turn
orient d1 d2 = toEnum $ fromEnum $ d2 - d1

allDirections :: [Direction]
allDirections = [North ..]

allTurns :: [Turn]
allTurns = [TNone ..]

instance Buildable Turn where
  build x = case x of
    TNone -> build "･･"
    TLeft -> build "<-"
    TRight -> build "->"
    TAround -> build "<>"

-- →←↑↓↔︎∙

instance Buildable Direction where
  build x = case x of
    North -> build "N"
    East -> build "E"
    South -> build "S"
    West -> build "W"
