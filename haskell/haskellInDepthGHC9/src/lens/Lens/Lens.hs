{-# LANGUAGE RankNTypes #-}

module Lens.Lens
  ( Optic,
    Optic',
    Lens,
    Over,
    Over',
    LensLike,
    LensLike',
    IndexedLensLike,
    module X,
  )
where

import Lens.Class as X

type Lens s t a b = forall f. Functor f => (a -> f b) -> s -> f t

type Optic p f s t a b = p a (f b) -> p s (f t)

type Optic' p f s a = Optic p f s s a a

type Over p f s t a b = p a (f b) -> s -> f t

type Over' p f s a = Over p f s s a a

type LensLike f s t a b = (a -> f b) -> s -> f t

type LensLike' f s a = LensLike f s s a a

type IndexedLensLike i f s t a b = forall p. Indexable i p => p a (f b) -> s -> f t
