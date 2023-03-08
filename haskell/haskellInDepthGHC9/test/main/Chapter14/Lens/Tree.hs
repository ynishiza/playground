{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use void" #-}

module Chapter14.Lens.Tree
  ( Tree(..),
  tvalue,
  tleft,
  tright,
  tVALUE,
  tLEFT,
  tRIGHT,
  phantom2,
  )
where

import Data.Functor.Contravariant
import Control.Lens

data Tree a where
  Leaf :: {_tvalue :: a} -> Tree a
  Node :: {_tleft :: Tree a, _tright :: Tree a} -> Tree a
  deriving stock (Eq, Show, Functor, Foldable, Traversable)

-- tVALUE :: Traversal' (Tree a) a
tVALUE :: Applicative f => (a -> f a) -> Tree a -> f (Tree a)
tVALUE f (Leaf v) = Leaf <$> f v
tVALUE f (Node l r) = Node <$> tVALUE f l <*> tVALUE f r

tLEFT :: Applicative f => (Tree a -> f (Tree a)) -> Tree a -> f (Tree a)
tLEFT f (Node t _) = f t
tLEFT _ t = pure t

tRIGHT :: Applicative f => (Tree a -> f (Tree a)) -> Tree a -> f (Tree a)
tRIGHT f (Node _ t) = f t
tRIGHT _ t = pure t

makeLenses ''Tree

phantom2 :: (Functor f, Contravariant f) => f a -> f b
phantom2 = contramap (const ()) . fmap (const ()) 
