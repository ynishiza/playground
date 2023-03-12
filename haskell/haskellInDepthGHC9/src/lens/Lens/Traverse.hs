{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}

module Lens.Traverse
  ( IndexedTraversal,
  element,
  )
where

import Lens.Class
import Lens.Monoids

type Traversal s t a b = forall f. Applicative f => (a -> f b) -> s -> (f t)

type IndexedTraversal i s t a b = forall p f. (Indexable i p, Applicative f) => p a (f b) -> s -> f t

type IndexedTraversal' i s a = IndexedTraversal i s s a a

elementOf :: Applicative f => LensLike (Indexing f) s t a a -> Int -> IndexedLensLike Int f s t a a
elementOf p i q = undefined

element :: Traversable t => Int -> IndexedTraversal' Int (t a) a
element = elementOf traverse
