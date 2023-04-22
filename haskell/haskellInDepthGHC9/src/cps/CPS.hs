{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda using `infix`" #-}

module CPS
  ( CMaybe (..),
    CMaybeT (..),
    CList (..),
    CFree (..),
    Cfn (..),
    Invertible (..),
  )
where

import Control.Monad
import Control.Monad.Free
import Control.Monad.Trans.Maybe

class Invertible m n | m -> n where
  from :: m a -> n a
  to :: n a -> m a

newtype CMaybe a = CMaybe {runCMaybe :: forall r. r -> (a -> r) -> r}

instance Functor CMaybe where
  fmap f (CMaybe m) = CMaybe $ \r k -> m r (k . f)

instance Applicative CMaybe where
  pure a = to (Just a)
  (<*>) = ap

instance Monad CMaybe where
  (CMaybe m) >>= h = CMaybe $ \r k -> m r (\a -> runCMaybe (h a) r k)

instance Invertible CMaybe Maybe where
  to :: Maybe a -> CMaybe a
  to (Just a) = CMaybe $ \_ k -> k a
  to Nothing = CMaybe const
  from :: CMaybe a -> Maybe a
  from (CMaybe m) = m Nothing Just

newtype CMaybeT m a = CMaybeT {runCMaybeT :: forall r. m r -> (a -> m r) -> m r}

instance Monad m => Invertible (CMaybeT m) (MaybeT m) where
  from (CMaybeT t) = MaybeT $ t (return Nothing) (return . Just)
  to (MaybeT s) = CMaybeT $ \r k -> s >>= maybe r k

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

newtype CFree f a = CFree {runCFree :: forall r. (a -> r) -> (f r -> r) -> r}

runCFreeWith :: (a -> r) -> (f r -> r) -> CFree f a -> r
runCFreeWith k kf (CFree m) = m k kf

instance Functor (CFree f) where
  fmap f (CFree m) = CFree $ \ka kf -> m (ka . f) kf

instance Applicative (CFree f) where
  pure a = CFree $ \ka _ -> ka a
  (<*>) = ap

instance Monad (CFree f) where
  (CFree m) >>= h = CFree $ \kb kf -> m (\a -> runCFree (h a) kb kf) kf

instance Functor f => Invertible (CFree f) (Free f) where
  from :: CFree f a -> Free f a
  from (CFree m) = m Pure Free
  to :: forall a. Free f a -> CFree f a
  to (Pure a) = CFree $ \ka _ -> ka a
  to (Free f) = CFree $ \ka kf -> kf (runCFreeWith ka kf . to <$> f)

newtype Cfn t a = Cfn {runCfn :: forall r. (a -> r) -> t -> r}

instance Invertible (Cfn t) ((->) t) where
  from (Cfn m) = m id
  to f = Cfn $ \k t -> k (f t)

instance Functor (Cfn t) where
  fmap f (Cfn m) = Cfn $ \k t -> m (k . f) t

instance Applicative (Cfn t) where
  pure a = Cfn $ \k _ -> k a
  (<*>) = ap

instance Monad (Cfn t) where
  (>>=) :: Cfn t a -> (a -> Cfn t b) -> Cfn t b
  (Cfn m) >>= h = Cfn $ \k t -> m (\a -> runCfn (h a) k t) t
