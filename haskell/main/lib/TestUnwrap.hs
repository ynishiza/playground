module TestUnwrap
  ( Elem (..),
    ElemMix (..),
    expandElem,
  )
where

data Elem = EInt Int | EStr String deriving (Eq, Show)

data ElemMix = EItem Elem | EList [ElemMix] deriving (Eq, Show)

expandElem :: [ElemMix] -> [[Elem]]
expandElem [] = [[]]
expandElem ((EItem x) : xs) = (x :) <$> expandElem xs
expandElem ((EList []) : _) = []      -- short circuit if empty
expandElem ((EList l) : xs) = do
  v <- l
  (++) <$> expandElem [v] <*> expandElem xs
