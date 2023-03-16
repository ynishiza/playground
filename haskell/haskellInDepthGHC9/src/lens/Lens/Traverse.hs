{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Lens.Traverse
  ( IndexedTraversal,
    Traversal,
    IndexedTraversal',
    element,
    elementOf,
    elementsOf,
    elements,
  )
where

import Control.Arrow ((>>>))
import Lens.Lens

type Traversal s t a b = forall f. Applicative f => (a -> f b) -> s -> f t

type IndexedTraversal i s t a b = forall p f. (Indexable i p, Applicative f) => p a (f b) -> s -> f t

type IndexedTraversal' i s a = IndexedTraversal i s s a a

elementOf :: Applicative f => LensLike (Indexing f) s t a a -> Int -> IndexedLensLike Int f s t a a
elementOf applyIndexing n = elementsOf applyIndexing (== n)

elementsOf :: Applicative f => LensLike (Indexing f) s t a a -> (Int -> Bool) -> IndexedLensLike Int f s t a a
elementsOf indexing predicate getA =
  indexing (\a -> Indexing $ \i -> (i + 1, if predicate i then indexed getA i a else pure a))
    >>> runIndexing
    >>> ($ 0)
    >>> snd

element :: Traversable t => Int -> IndexedTraversal' Int (t a) a
element = elementOf traverse

elements :: Traversable t => (Int -> Bool) -> IndexedTraversal' Int (t a) a
elements = elementsOf traverse
