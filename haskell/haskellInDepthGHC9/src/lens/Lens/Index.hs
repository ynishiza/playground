{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Lens.Index
  ( Indexed (..),
    selfIndex,
    reindex,
    indexing,
    indices,
    index,
  )
where

import Control.Arrow ((>>>))
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

instance Indexable i (Indexed i) where
  indexed = runIndexed

selfIndex :: Indexable a p => p a b -> a -> b
selfIndex pab a = indexed pab a a

reindex :: Indexable j p => (i -> j) -> (Indexed i a b -> r) -> p a b -> r
reindex f lens pab = lens $ Indexed $ \i a -> indexed pab (f i) a

indexing :: Indexable Int p => LensLike (Indexing f) s t a b -> Over p f s t a b
indexing lens pafb =
  lens f
    >>> execIndexing 0
  where
    f a = Indexing $ \i -> (i + 1, indexed pafb i a)

indices :: (Indexable i p, Applicative f) => (i -> Bool) -> Optical' p (Indexed i) f a a
indices predicate pafa = Indexed $ \i a -> if predicate i then indexed pafa i a else pure a

index :: (Indexable i p, Eq i, Applicative f) => i -> Optical' p (Indexed i) f a a
index i = indices (== i)
