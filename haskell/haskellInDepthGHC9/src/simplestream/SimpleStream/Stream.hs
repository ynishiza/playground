{-# HLINT ignore "Use <&>" #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{- ORMOLU_DISABLE -}
module SimpleStream.Stream
  ( Stream (..),
  empty,
  yields,
  effect,
  replicates,
  repeats,
  repeatsM,
  unfold,
  maps, 
  mapsM,
  takes,
  zipsWith,
  zipCompose,
  decompose,
  splitsAt,
  ChunkedStream,
  chunks,
  concats,
  joins,
  copy,
  store,

  module X,
  )
where
{- ORMOLU_ENABLE -}

import Control.Exception
import Control.Monad as X
import Control.Monad.Free.Class
import Control.Monad.IO.Class as X
import Control.Monad.Trans.Maybe as X
import Data.Bifunctor as X
import Data.Functor.Compose as X
import Data.Kind
import Data.Monoid as X
import Fmt

type Stream :: (Type -> Type) -> (Type -> Type) -> Type -> Type
data Stream f m r where
  Return :: r -> Stream f m r
  Step :: Functor f => f (Stream f m r) -> Stream f m r
  Effect :: Monad m => m (Stream f m r) -> Stream f m r

instance Functor f => MonadFree f (Stream f m) where
  wrap = Step

instance Functor (Stream f m) where
  fmap :: forall a b. (a -> b) -> Stream f m a -> Stream f m b
  fmap f = loop
    where
      loop :: Stream f m a -> Stream f m b
      loop (Return r) = Return $ f r
      loop (Step s) = wrap $ loop <$> s
      loop (Effect e) = Effect $ loop <$> e

instance Applicative (Stream f m) where
  pure = Return
  (<*>) = ap

instance Monad (Stream f m) where
  return = pure
  (>>=) :: forall a b. Stream f m a -> (a -> Stream f m b) -> Stream f m b
  m >>= k = loop m
    where
      loop :: Stream f m a -> Stream f m b
      loop (Return r) = k r
      loop (Step s) = wrap $ loop <$> s
      loop (Effect e) = Effect $ loop <$> e

instance MonadIO m => MonadIO (Stream f m) where
  liftIO :: IO a -> Stream f m a
  liftIO io = Effect $ do
    x <- liftIO io
    return $ Return x

-- ==================== Utils  ====================

empty :: Stream f m ()
empty = Return ()

effect :: Monad m => m r -> Stream f m r
effect e = Effect $ Return <$> e

yields :: (Functor f) => f r -> Stream f m r
yields v = Step $ Return <$> v

replicates :: (Monad m, Functor f) => Int -> f () -> Stream f m ()
replicates n = takes n . repeats 

repeats :: (Functor f) => f () -> Stream f m ()
repeats v = yields v >> repeats v

repeatsM :: (Monad m, Functor f) => m (f ()) -> Stream f m ()
repeatsM v = Effect (yields <$> v) >> repeatsM v

unfold :: (Monad m, Functor f) => (s -> m (Either r (f s))) -> s -> Stream f m r
unfold fn s = Effect $ fn s >>= return . either Return step
  where
    step f = Step $ unfold fn <$> f

never :: (Monad m, Applicative f) => Stream f m r
never = undefined

maps :: forall f g m r. (Functor g) => (forall x. f x -> g x) -> Stream f m r -> Stream g m r
maps fn = loop
  where
    loop :: Stream f m r -> Stream g m r
    loop (Return r) = Return r
    loop (Step s) = Step $ fn $ loop <$> s
    loop (Effect e) = Effect $ loop <$> e

zipsWith :: forall f g h m r. Functor h => (forall x y z. (x -> y -> z) -> f x -> g y -> h z) -> Stream f m r -> Stream g m r -> Stream h m r
zipsWith fn = loop
  where
    loop :: Stream f m r -> Stream g m r -> Stream h m r
    loop (Return r) _ = Return r
    loop _ (Return r) = Return r
    loop (Effect e) s = Effect $ flip loop s <$> e
    loop s (Effect e) = Effect $ loop s <$> e
    loop (Step s1) (Step s2) = Step $ fn loop s1 s2

zipCompose :: (Functor f, Functor g) => Stream f m r -> Stream g m r -> Stream (Compose f g) m r
zipCompose = zipsWith (\fn s1 s2 -> Compose $ (<$> s2) <$> (fn <$> s1))

decompose :: forall f m r. (Monad m, Functor f) => Stream (Compose m f) m r -> Stream f m r
decompose (Return r) = Return r
decompose (Effect e) = Effect $ decompose <$> e
decompose (Step s) = Effect $ getCompose s >>= pure . Step . (decompose <$>)

mapsM :: forall f g m r. (Functor g, Monad m) => (forall x. f x -> m (g x)) -> Stream f m r -> Stream g m r
mapsM fn = decompose . maps (Compose . fn)

takes :: (Monad m, Functor f) => Int -> Stream f m r -> Stream f m ()
takes n str = void $ splitsAt n str

splitsAt :: (Monad m, Functor f) => Int -> Stream f m r -> Stream f m (Stream f m r)
splitsAt n str
  | n > 0 = case str of
      (Return _) -> Return str
      (Effect e) -> Effect $ splitsAt n <$> e
      (Step s) -> Step $ splitsAt (n - 1) <$> s
  | otherwise = Return str

type ChunkedStream f m = Stream (Stream f m) m

chunks :: forall f m r. (Monad m, Functor f) => Int -> Stream f m r -> ChunkedStream f m r
chunks n
  | n <= 0 = throw $ userError $ "chunk size must be > 0 but received " +| n |+ ""
  | otherwise = loop
  where
    loop :: Stream f m r -> ChunkedStream f m r
    loop (Return r) = Return r
    loop (Effect e) = Effect $ loop <$> e
    loop (Step s) = Step $ Step $ chunkN s

    -- chunk N by
    --      1   +       N - 1
    --      f       (Stream f m *)
    chunkN :: f (Stream f m r) -> f (Stream f m (ChunkedStream f m r))
    chunkN s = (loop <$>) . splitsAt (n - 1) <$> s

joins :: forall f m r. Stream f m (Stream f m r) -> Stream f m r
joins (Return r) = r
joins (Effect e) = Effect $ joins <$> e
joins (Step s) = Step $ joins <$> s

concats :: forall f m r. Stream (Stream f m) m r -> Stream f m r
concats (Return r) = Return r
concats (Effect e) = Effect $ concats <$> e
concats (Step (Return r)) = concats r
concats (Step (Effect e)) = Effect $ joins . (concats <$>) <$> e
concats (Step (Step s)) = Step $ joins . (concats <$>) <$> s

copy :: forall f m r. Stream f m r -> Stream f (Stream f m) r
copy (Return r) = Return r
copy (Step s) = Effect $ Step z
  where
    z = (copy <$>) . copyReturn <$> s
copy (Effect e) = Effect y
  where
    z = (copy <$>) . copyReturn <$> e
    y = Effect z

copyReturn :: Stream f m r -> Stream f m (Stream f m r)
copyReturn s@(Return _) = Return s
copyReturn (Step s) = do
  r <- Step $ copyReturn <$> s
  error "BROKEN"
  return $ r >> Step s
-- return $ r >> Step s
copyReturn (Effect e) = do
  r <- Effect $ copyReturn <$> e
  error "BROKEN"
  return $ r >> Effect e

store :: (Stream f (Stream f m) r -> t) -> Stream f m r -> t
store f = f . copy
