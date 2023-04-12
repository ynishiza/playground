{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}

module Tree
  ( Tree (..),
    _leaf,
    _left,
    _right,
  )
where

import Data.Function ((&))
import Lens

data Tree a where
  Leaf :: a -> Tree a
  Node :: Tree a -> Tree a -> Tree a
  deriving stock (Show, Eq, Ord, Foldable, Functor, Traversable)

ifLeaf :: (a -> r) -> r -> Tree a -> r
ifLeaf f _ (Leaf a) = f a
ifLeaf _ g _ = g

ifNode :: (Tree a -> Tree a -> r) -> r -> Tree a -> r
ifNode f _ (Node l r) = f l r
ifNode _ g _ = g

_leaf :: (ProfunctorArrow p, Applicative f) => Optic p f (Tree a) (Tree a) a a
_leaf p =
  lmap (ifLeaf id (error "Should never happen")) p
    & strong (\tree x -> ifLeaf (const (Leaf <$> x)) (pure tree) tree)

_left :: (ProfunctorArrow p, Applicative f) => Optic' p f (Tree a) (Tree a)
_left p =
  lmap (ifNode const (error "Should never happen")) p
    & strong (\tree x -> ifNode (\_ r -> flip Node r <$> x) (pure tree) tree)

_right :: forall f a. Applicative f => LensLike f (Tree a) (Tree a) (Tree a) (Tree a)
_right p =
  lmap (ifNode (\_ r -> r) (error "Should never happen")) p
    & strong (\tree x -> ifNode (\l _ -> Node l <$> x) (pure tree) tree)
