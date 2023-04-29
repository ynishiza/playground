{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
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

    both,
    beside,
    partsOf,
  )
where
{- ORMOLU_ENABLE -}

import Control.Arrow ((>>>))
import Data.Bitraversable
import Data.Function
import Data.Functor.Compose
import Lens.Lens
import Lens.Monoid

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

both :: Bitraversable r => Traversal (r a a) (r b b) a b
both k = bitraverse k k

beside ::
  (ProfunctorRepresentation q, Applicative (Rep q), Applicative f, Bitraversable r) =>
  Optical p q f s t a b ->
  Optical p q f s' t' a b ->
  Optical p q f (r s s') (r t t') a b
beside lens1 lens2 k =
  fromRep
    ( \r ->
        bitraverse
          (Compose . toRep (lens1 k))
          (Compose . toRep (lens2 k))
          r
          & getCompose
    )

partsOf :: forall f s t a. Applicative f => LensLike (Cap a a) s t a a -> LensLike f s t [a] [a]
partsOf lens k s =
  lens collect s
    & mp k
  where
    collect :: a -> Cap a a a
    collect = CVal
    mp :: ([a] -> f [a]) -> Cap a a t -> f t
    mp f c =
      captureToList c
        & f
        & (captureMapToX c <$>)

data Cap a t x where
  CPure :: b -> Cap a t b
  Cfmap :: (x -> y) -> Cap a t x -> Cap a t y
  CAp :: Cap a t (x -> y) -> Cap a t x -> Cap a t y
  CVal :: a -> Cap a x x

instance Functor (Cap a t) where
  fmap :: (x -> y) -> Cap a t x -> Cap a t y
  fmap = Cfmap

instance Applicative (Cap a t) where
  pure = CPure
  f <*> x = CAp f x

newtype CapD x t a = CapD (Cap a t x)

instance Foldable (CapD x t) where
  foldr :: (a -> r -> r) -> r -> CapD x t a -> r
  foldr g r (CapD d) = case d of
    CPure _ -> r
    CVal a -> g a r
    Cfmap _ x -> foldr g r (CapD x)
    CAp f x -> foldr g (foldr g r (CapD f)) (CapD x)

instance Functor (CapD x t) where
  fmap :: (a -> b) -> CapD x t a -> CapD x t b
  fmap f (CapD d) = case d of
    CPure v -> CapD $ CPure v
    CVal v -> CapD $ CVal $ f v
    Cfmap g x -> CapD $ Cfmap g y
      where
        (CapD y) = fmap f (CapD x)
    CAp g x -> CapD $ CAp g' x'
      where
        (CapD g') = fmap f (CapD g)
        (CapD x') = fmap f (CapD x)

captureToList :: Cap a t x -> [a]
captureToList (CVal a) = [a]
captureToList (Cfmap _ x) = captureToList x
captureToList (CAp f x) = captureToList f ++ captureToList x
captureToList _ = []

captureToX :: Cap a a x -> x
captureToX (CVal x) = x
captureToX (CPure x) = x
captureToX (Cfmap f x) = f (captureToX x)
captureToX (CAp f x) = captureToX f (captureToX x)

captureMap :: ([a], Cap a a x) -> ([a], x)
captureMap ([], c) = ([], captureToX c)
captureMap (a : xs, CVal _) = (xs, a)
captureMap (as, CPure v) = (as, v)
captureMap (as, Cfmap f x) = (as', f x')
  where
    (as', x') = captureMap (as, x)
captureMap (as, CAp f x) = (as'', f' x')
  where
    (as', f') = captureMap (as, f)
    (as'', x') = captureMap (as', x)

captureMapToX :: Cap a a x -> [a] -> x
captureMapToX c as =
  captureMap (as, c)
    & snd
