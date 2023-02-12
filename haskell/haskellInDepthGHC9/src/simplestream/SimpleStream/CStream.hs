{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}
{- ORMOLU_DISABLE -}
module SimpleStream.CStream (
  CStream(..),
  toStream,
  fromStream,
  yieldc,
  effectc,

  mapc,
  mapcM,

  runCStreamWith,
  replicatec,
  groupc,

  takec,
  splitAtc,
  chunkc,

  mapcM_,
  run,
  ) where
{- ORMOLU_ENABLE -}

import Control.Exception
import Control.Monad
import Control.Monad.Free
import Control.Monad.Trans.Class
import Data.Functor.Sum
import SimpleStream.Stream qualified as S

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
  lift = wrapE . (pure <$>)

instance Functor f => MonadFree f (CStream f m) where
  wrap :: f (CStream f m a) -> CStream f m a
  wrap f = CStream $ \kp ks ke -> ks (runCStreamWith kp ks ke <$> f)

wrapE :: Monad m => m (CStream f m r) -> CStream f m r
wrapE f = CStream $ \kp ks ke -> ke $ runCStreamWith kp ks ke <$> f

toStream :: CStream f m r -> S.Stream f m r
toStream (CStream s) = s S.Return S.Step S.Effect

fromStream :: (Functor f, Monad m) => S.Stream f m r -> CStream f m r
fromStream = S.streamFold return wrap wrapE

yieldc :: Functor f => f r -> CStream f m r
yieldc = liftF

effectc :: Monad m => m r -> CStream f m r
effectc = lift

replicatec :: (Functor f, Monad m) => Int -> CStream f m r -> CStream f m r
replicatec n c
  | n > 0 = c >> replicatec (n - 1) c
  | otherwise = c

groupc :: forall m f g r. (Monad m, Functor f, Functor g) => CStream (Sum f g) m r -> CStream (Sum (CStream f m) (CStream g m)) m r
groupc (CStream str) = str pure ks wrapE
  where
    ks :: Sum f g (CStream (Sum (CStream f m) (CStream g m)) m r) -> CStream (Sum (CStream f m) (CStream g m)) m r
    ks (InL m) = wrap $ InL $ yieldc m
    ks (InR m) = wrap $ InR $ yieldc m

mapc :: (Functor g, Monad m) => (forall x. f x -> g x) -> CStream f m r -> CStream g m r
mapc mp = mapcM (pure . mp)

mapcM :: (Functor g, Monad m) => (forall x. f x -> m (g x)) -> CStream f m r -> CStream g m r
mapcM mp (CStream c) = c pure (wrapE . (wrap <$>) . mp) wrapE

takec :: (Functor f, Monad m) => Int -> CStream f m r -> CStream f m ()
takec n = (const () <$>) . splitAtc n

splitAtc :: forall f m r. (Functor f, Monad m) => Int -> CStream f m r -> CStream f m (CStream f m r)
splitAtc n (CStream c)
  | n < 0 = throw $ userError "n must be non-negative"
  | otherwise = c kp ks ke 0
  where
    kp :: r -> Int -> CStream f m (CStream f m r)
    kp r _ = pure (pure r)
    ks :: f (Int -> CStream f m (CStream f m r)) -> Int -> CStream f m (CStream f m r)
    ks f m = cut (($ m + 1) <$> f) m
    ke :: m (Int -> CStream f m (CStream f m r)) -> Int -> CStream f m (CStream f m r)
    ke e m = wrapE $ ($ m) <$> e
    cut :: f (CStream f m (CStream f m r)) -> Int -> CStream f m (CStream f m r)
    cut f n' = if n' < n then x else return (join x)
      where
        x = wrap f

isEmpty :: Monad m => CStream f m r -> CStream g m Bool
isEmpty (CStream c) = c (const $ pure True) (const $ pure False) wrapE

chunkc :: forall f m r. (Functor f, Monad m) => Int -> CStream f m r -> CStream (CStream f m) m r
chunkc n
  | n <= 0 = throw $ userError "n must be positive"
  | otherwise = loop
  where
    loop :: CStream f m r -> CStream (CStream f m) m r
    loop s = do
      -- TODO: better way to check if empty?
      let x = splitAtc n s
      e <- isEmpty x
      if e then runCStream s pure (wrap . yieldc) wrapE else wrap $ loop <$> x

mapcM_ :: Monad m => (forall x. f x -> m x) -> CStream f m r -> m r
mapcM_ ps (CStream str) = str pure (join . ps) join

run :: Monad m => CStream m m r -> m r
run (CStream str) = str pure join join

zipcWith' :: forall m f r g h. Monad m => (forall x y z. (x -> y -> z) -> f x -> g y -> h z) -> CStream f m r -> CStream g m r -> CStream h m r
zipcWith' mp = undefined
