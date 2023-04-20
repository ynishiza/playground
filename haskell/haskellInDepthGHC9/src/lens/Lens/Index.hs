{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE TypeFamilies #-}

module Lens.Index
  ( Indexed (..),
    selfIndex,
    reindex,
    indexing,
    indices,
    index,
    withIndex,
    asIndex,
    (<.>),
    (.>),
    (<.),
  )
where

import Data.Function ((&))
import Lens.Class
import Lens.Lens

-- note: Indexed function
newtype Indexed i a b where
  Indexed :: {runIndexed :: i -> a -> b} -> Indexed i a b
  deriving (Functor)

instance Applicative (Indexed i a) where
  pure = Indexed . const . const
  (Indexed f) <*> (Indexed x) = Indexed $ \i a -> f i a $ x i a

instance Profunctor (Indexed i) where
  dimap g h (Indexed f) = Indexed $ \i -> h . f i . g

instance ProfunctorArrow (Indexed i) where
  arr' f = Indexed $ \_ a -> f a
  first' (Indexed f) = Indexed $ \i (a, c) -> (f i a, c)

instance ProfunctorChoice (Indexed i) where
  left' (Indexed f) = Indexed $ \i x -> case x of
    (Left a) -> Left $ f i a
    (Right c) -> Right c

-- NOTE: i ~ j for type inference
--
--    instance Indexable i (Indexed i)                  BAD  instance Indexable i (Indexed j) exists only if i == j
--    instance i ~ j => Indexable i (Indexed j)         GOOD instance Indexable i (Indexed j) exists for any i, j but valid only if i == j
--
-- e.g.
--    -- with i ~ j
--    itoListOf folded [True, False]                    OK. [(0, True), (1, False)]
--
--    -- without i ~ j
--    itoListOf folded [True, False]                    ERROR. "Ambiguous type variable ..."
--
instance i ~ j => Indexable i (Indexed j) where
  indexed = runIndexed

instance ProfunctorRepresentation (Indexed i) where
  type Rep (Indexed i) = (->) i
  toRep (Indexed f) a i = f i a
  fromRep f = Indexed $ \i a -> f a i

-- use index of outer
(<.) :: Indexable i p => (Indexed i s t -> r) -> ((a -> b) -> s -> t) -> p a b -> r
(<.) lens1 lens2 ka = lens1 $
  Indexed $
    \i -> lens2 (indexed ka i)

-- zip indices 
(<.>) :: Indexable (i, j) p => (Indexed i s t -> r) -> (Indexed j a b -> s -> t) -> p a b -> r
(<.>) = icompose (,)

-- use index of inner
(.>) :: ((s -> t) -> r) -> (p a b -> s -> t) -> p a b -> r
(.>) lens1 lens2 ka =
  lens1 $
    lens2 ka

icompose :: Indexable k p => (i -> j -> k) -> (Indexed i s t -> r) -> (Indexed j a b -> s -> t) -> p a b -> r
icompose f lens1 lens2 ka = lens1 $ 
  Indexed $ \i -> lens2 $ 
    Indexed $ \j -> indexed ka (f i j)

selfIndex :: Indexable a p => p a b -> a -> b
selfIndex ka a = indexed ka a a

reindex :: Indexable j p => (i -> j) -> (Indexed i a b -> r) -> p a b -> r
reindex f lens ka = lens $ Indexed $ \i a -> indexed ka (f i) a


indexing :: Indexable Int p => LensLike (Indexing f) s t a b -> Over p f s t a b
indexing lens ka s =
  lens run s
    & execIndexing 0
  where
    run a = Indexing $ \i -> (i + 1, indexed ka i a)

-- traverse indices
asIndex :: (Indexable i p, Functor f) => p i (f i) -> Indexed i s (f s)
asIndex ki = Indexed $ \i s -> s <$ indexed ki i i

-- traverse (index, value) pair
withIndex :: (Indexable i p, Functor f) => p (i, s) (f (j, t)) -> Indexed i s (f t)
withIndex ki = Indexed $ \i s -> snd <$> indexed ki i (i, s)

indices :: (Indexable i p, Applicative f) => (i -> Bool) -> Optical' p (Indexed i) f a a
indices predicate pafa = Indexed $ \i a -> if predicate i then indexed pafa i a else pure a

index :: (Indexable i p, Eq i, Applicative f) => i -> Optical' p (Indexed i) f a a
index i = indices (== i)
