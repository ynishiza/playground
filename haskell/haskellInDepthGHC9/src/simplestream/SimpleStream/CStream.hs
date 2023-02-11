{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE InstanceSigs #-}
{- ORMOLU_DISABLE -}
module SimpleStream.CStream (
  CStream(..),
  toStream,
  fromStream,
  yieldc,
  effectc,

  runCStreamWith,
  replicatec,
  takec,
  ) where
{- ORMOLU_ENABLE -}

import Control.Monad
import Control.Monad.Free
import Control.Monad.Trans.Class
import SimpleStream.Stream

newtype CStream f m r where
  CStream :: {runCStream :: forall a. (r -> a) -> (f a -> a) -> (m a -> a) -> a} -> CStream f m r

runCStreamWith :: (r -> a) -> (f a -> a) -> (m a -> a) -> CStream f m r -> a
runCStreamWith kp ks km (CStream c) = c kp ks km

instance Functor (CStream f m) where
  fmap f (CStream c) = CStream $ \kp ks ke -> c (kp . f) ks ke

instance Applicative (CStream f m) where
  pure r = CStream $ \kp _ _ -> kp r
  (<*>) = ap

instance Monad (CStream f m) where
  (CStream c) >>= k = CStream $ \kp ks ke -> c (\r -> runCStream (k r) kp ks ke) ks ke

instance MonadTrans (CStream f) where
  lift :: Monad m => m a -> CStream f m a
  lift = effectc

instance Functor f => MonadFree f (CStream f m) where
  wrap :: f (CStream f m a) -> CStream f m a
  wrap f = CStream $ \kp ks ke -> ks (runCStreamWith kp ks ke <$> f)

wrapE :: Monad m => m (CStream f m r) -> CStream f m r
wrapE f = CStream $ \kp ks ke -> ke $ runCStreamWith kp ks ke <$> f

toStream :: CStream f m r -> Stream f m r
toStream (CStream s) = s Return Step Effect

fromStream :: (Functor f, Monad m) => Stream f m r -> CStream f m r
fromStream = streamFold return wrap wrapE

yieldc :: Functor f => f r -> CStream f m r
yieldc = wrap . (pure <$>)

effectc :: Monad m => m r -> CStream f m r
effectc = wrapE . (pure <$>)

replicatec :: (Monad m, Functor f) => Int -> CStream f m r -> CStream f m r
replicatec n c
  | n > 0 = c >> replicatec (n - 1) c
  | otherwise = c

takec :: forall m f r. (Monad m, Functor f) => Int -> CStream f m r -> CStream f m ()
takec n (CStream c) = c p step eff 0
  where
    p :: r -> Int -> CStream f m ()
    p _ _ = pure ()
    step :: f (Int -> CStream f m ()) -> Int -> CStream f m ()
    step f m = cut (wrap $ ($ (m + 1)) <$> f) m
    eff :: m (Int -> CStream f m ()) -> Int -> CStream f m ()
    eff m _ = wrapE $ ($ 0) <$> m
    cut :: CStream f m () -> Int -> CStream f m ()
    cut c' n' = when (n' < n) $ void c' 
