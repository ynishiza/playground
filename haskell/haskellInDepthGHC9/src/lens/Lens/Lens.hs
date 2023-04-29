{-# LANGUAGE RankNTypes #-}

{- ORMOLU_DISABLE -}
module Lens.Lens
  ( 
    Setter,
    Fold,
    Getter,
    Traversal,
    Prism,
    Prism',
    Lens,
    Lens',

    IndexedSetter,
    IndexedSetter',
    IndexPreservingSetter,
    IndexedFold,
    IndexedTraversal,
    IndexedTraversal',

    Optical,
    Optical',
    Optic,
    Optic',
    Over,
    Over',
    LensLike,
    LensLike',
    IndexedLensLike,
    module X,
  )
where
{- ORMOLU_ENABLE -}

import Data.Functor.Contravariant
import Lens.Class as X

type Setter s t a b = forall f. Settable f => (a -> f b) -> s -> f t

type Fold s a = forall f. (Contravariant f, Applicative f) => (a -> f a) -> s -> f s

type Getter s t a b = forall f. (Contravariant f, Functor f) => (a -> f b) -> s -> f t

type Traversal s t a b = forall f. Applicative f => (a -> f b) -> s -> f t

type Prism s t a b = forall p f. (ProfunctorChoice p, Applicative f) => p a (f b) -> p s (f t)

type Prism' s a = Prism s s a a

type Lens s t a b = forall f. Functor f => (a -> f b) -> s -> f t

type Lens' s a = Lens s s a a

-- Indexed

type IndexedSetter i s t a b = forall p f. (Indexable i p, Settable f) => p a (f b) -> s -> f t

type IndexedSetter' i s a = IndexedSetter i s s a a

type IndexPreservingSetter s t a b = forall p f. (ProfunctorRepresentation p, Settable f) => p a (f b) -> p s (f t)

type IndexedFold i s a = forall f p. (Indexable i p, Contravariant f, Applicative f) => p a (f a) -> s -> f s

type IndexedTraversal i s t a b = forall p f. (Indexable i p, Applicative f) => p a (f b) -> s -> f t

type IndexedTraversal' i s a = IndexedTraversal i s s a a

-- General

type Optical p q f s t a b = p a (f b) -> q s (f t)

type Optical' p q f s a = Optical p q f s s a a

type Optic p f s t a b = p a (f b) -> p s (f t)

type Optic' p f s a = Optic p f s s a a

type Over p f s t a b = p a (f b) -> s -> f t

type Over' p f s a = Over p f s s a a

type LensLike f s t a b = (a -> f b) -> s -> f t

type LensLike' f s a = LensLike f s s a a

type IndexedLensLike i f s t a b = forall p. Indexable i p => p a (f b) -> s -> f t
