{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}

module Tree
  ( Tree (..),
    _leaf,
    _left,
    _right,
  )
where

import Lens

data Tree a where
  Leaf :: a -> Tree a
  Node :: Tree a -> Tree a -> Tree a
  deriving stock (Show, Eq, Ord, Foldable, Functor, Traversable)

_leaf :: forall f a. Applicative f => LensLike f (Tree a) (Tree a) a a
_leaf ref (Leaf a) = Leaf <$> ref a
_leaf _ t = pure t

_left :: forall f a. Applicative f => LensLike f (Tree a) (Tree a) (Tree a) (Tree a)
_left ref (Node l r) = flip Node r <$> ref l
_left _ t = pure t

_right :: forall f a. Applicative f => LensLike f (Tree a) (Tree a) (Tree a) (Tree a)
_right ref (Node l r) = Node l <$> ref r
_right _ t = pure t
