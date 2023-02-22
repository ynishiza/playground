{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}

module CPS.List (CList (..), (|:), nil) where

import CPS.Class
import Control.Monad

newtype CList a = CList {runCList :: forall r. r -> (a -> r -> r) -> r}

instance Functor CList where
  fmap f (CList m) = CList $ \r k -> m r (k . f)

instance Applicative CList where
  pure a = CList $ \r k -> k a r
  (<*>) = ap

instance Monad CList where
  (CList m) >>= h = CList $ \r k -> m r (\a as -> runCList (h a) as k)

instance Invertible CList [] where
  from :: CList a -> [a]
  from (CList m) = m [] (:)
  to :: [a] -> CList a
  to l = CList $ \r k -> foldr k r l

instance Foldable CList where
  foldr f r (CList m) = m r f

instance Traversable CList where
  traverse f (CList m) = m (pure nil) (\a r -> (|:) <$> f a <*> r)

(|:) :: a -> CList a -> CList a
a |: (CList m) = CList $ \r k -> k a $ m r k

nil :: CList a
nil = CList const

infixr 1 |:
