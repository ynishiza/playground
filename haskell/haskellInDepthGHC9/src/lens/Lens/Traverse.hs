{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

{- ORMOLU_DISABLE -}
module Lens.Traverse
  ( IndexedTraversal,
    Traversal,
    IndexedTraversal',
    fromTraverse,
    toTraverse,
    traversal,
    traversed,
    traverseOf,
    elementOf,
    elementsOf,
    element,
    elements,
  )
where
{- ORMOLU_ENABLE -}

import Control.Arrow ((>>>))
import Lens.Lens

fromTraverse :: ((a -> f b) -> s -> f t) -> LensLike f s t a b
fromTraverse = id

toTraverse :: LensLike f s t a b -> (a -> f b) -> s -> f t
toTraverse = id

traversal :: ((a -> f b) -> s -> f t) -> LensLike f s t a b
traversal = fromTraverse

traversed :: Traversable t => IndexedTraversal Int (t a) (t b) a b
traversed pafb =
  traverse (\a -> Indexing $ \i -> (i + 1, indexed pafb i a))
    >>> execIndexing 0

traverseOf :: LensLike f s t a b -> (a -> f b) -> s -> f t
traverseOf = toTraverse

elementOf :: Applicative f => LensLike (Indexing f) s t a a -> Int -> IndexedLensLike Int f s t a a
elementOf lens n = elementsOf lens (== n)

elementsOf :: Applicative f => LensLike (Indexing f) s t a a -> (Int -> Bool) -> IndexedLensLike Int f s t a a
elementsOf lens predicate afb =
  lens pick
    >>> execIndexing 0
  where
    pick a = Indexing $ \i -> (i + 1, if predicate i then indexed afb i a else pure a)

element :: Traversable t => Int -> IndexedTraversal' Int (t a) a
element = elementOf traverse

elements :: Traversable t => (Int -> Bool) -> IndexedTraversal' Int (t a) a
elements = elementsOf traverse
