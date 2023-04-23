{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}

module Tree
  ( Tree (..),
    _Leaf,
    _LeftTree,
    _RightTree,
    _LeftTree',
    _RightTree',
    _Node,
  )
where

import Data.Function ((&))
import Lens

data Tree a where
  Leaf :: a -> Tree a
  Node :: Tree a -> Tree a -> Tree a
  deriving stock (Show, Eq, Ord, Foldable, Functor, Traversable)

_Leaf :: (ProfunctorChoice p, Applicative f) => Optic p f (Tree a) (Tree a) a a
_Leaf =
  prism
    (\tree -> case tree of
                Node _ _ -> Left tree
                Leaf a -> Right a)
    Leaf

_LeftTree :: (ProfunctorChoice p, ProfunctorArrow p, Applicative f) => Optic' p f (Tree a) (Tree a)
_LeftTree = _Branch True

_RightTree :: (ProfunctorChoice p, ProfunctorArrow p, Applicative f) => Optic' p f (Tree a) (Tree a)
_RightTree = _Branch False

_Branch :: (ProfunctorChoice p, ProfunctorArrow p, Applicative f) => Bool -> Optic' p f (Tree a) (Tree a)
_Branch isLeft k =
  right' k
    & lmap
      ( \tree -> case tree of
          Node l r -> Right $ if isLeft then l else r
          _ -> Left tree
      )
    & strong
      ( \tree b -> case b of
          Left t -> pure t
          Right x -> merge tree <$> x
      )
  where
    merge (Node l r) t = if isLeft then Node t r else Node l t
    merge _ _ = error ""

_LeftTree' :: Applicative f => LensLike' f (Tree a) (Tree a)
_LeftTree' k (Node l r) = flip Node r <$> k l
_LeftTree' _ l@(Leaf _) = pure l

_RightTree' :: Applicative f => LensLike' f (Tree a) (Tree a)
_RightTree' k (Node l r) = Node l <$> k r
_RightTree' _ l@(Leaf _) = pure l

_Node :: Prism' (Tree a) (Tree a, Tree a)
_Node =
  prism
    (\tree -> case tree of
                Node l r -> Right (l, r)
                Leaf _ -> Left tree)
    (uncurry Node)
