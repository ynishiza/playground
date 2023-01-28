{-# HLINT ignore "Use <&>" #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{- ORMOLU_DISABLE -}
module SimpleStream.Stream
  ( Stream (..),
  empty_,
  yields,
  effect,
  replicates,
  repeats,
  repeatsM,
  unfold,
  never,
  untilJust,
  delays,

  maps, 
  mapsPost,
  mapsM,
  mapsMPost,
  mapsMWithDecompose,
  groups,
  takes,

  ChunkedStream,
  splitsAt,
  chunks,
  concats,
  joins,
  -- copy,
  -- store,

  inspect, 

  zipsWith',
  zips,
  unzips,
  decompose,

  cp,
  mapsM_,
  run,
  iterTM,
  iterT,
  streamFold,
  collects,
  )
where
{- ORMOLU_ENABLE -}

import Data.Functor.Sum
import Control.Monad.Trans.Class
import Control.Applicative as X
import Control.Concurrent
import Control.Exception
import Control.Monad as X
import Control.Monad.Free.Class
import Control.Monad.IO.Class as X
import Data.Bifunctor as X
import Data.Functor.Compose as X
import Data.Kind
import Fmt

type Stream :: (Type -> Type) -> (Type -> Type) -> Type -> Type
data Stream f m r where
  Return :: r -> Stream f m r
  Step :: f (Stream f m r) -> Stream f m r
  Effect :: Monad m => m (Stream f m r) -> Stream f m r

instance Functor f => MonadFree f (Stream f m) where
  wrap = Step

instance Functor f => Functor (Stream f m) where
  fmap :: forall a b. (a -> b) -> Stream f m a -> Stream f m b
  fmap f = loop
    where
      loop :: Stream f m a -> Stream f m b
      loop (Return r) = Return $ f r
      loop (Step s) = wrap $ loop <$> s
      loop (Effect e) = Effect $ loop <$> e

instance Functor f => Applicative (Stream f m) where
  pure = Return
  (<*>) = ap

instance Functor f => Monad (Stream f m) where
  return = pure
  (>>=) :: forall a b. Stream f m a -> (a -> Stream f m b) -> Stream f m b
  m >>= k = loop m
    where
      loop :: Stream f m a -> Stream f m b
      loop (Return r) = k r
      loop (Step s) = wrap $ loop <$> s
      loop (Effect e) = Effect $ loop <$> e

instance (MonadIO m, Functor f) => MonadIO (Stream f m) where
  liftIO :: IO a -> Stream f m a
  liftIO io = Effect $ do
    x <- liftIO io
    return $ Return x

instance (Monad m, Applicative f) => Alternative (Stream f m) where
  empty = never
  (<|>) :: Stream f m r -> Stream f m r -> Stream f m r
  (<|>) = zipsWith $ \f g -> (,) <$> f <*> g

-- ==================== Section: Constructing a stream  ====================
-- https://hackage.haskell.org/package/streaming-0.2.3.1/docs/Streaming.html#g:2

empty_ :: Stream f m ()
empty_ = Return ()

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
never = Step $ pure never

untilJust :: (Monad m, Applicative f) => m (Maybe r) -> Stream f m r
untilJust m = Effect $ m >>= return . maybe never Return

delays :: (MonadIO m) => Int -> Stream f m ()
delays n = Effect $ liftIO (threadDelay n) >> return (Return ())

-- ==================== Section: transforming a stream  ====================
-- https://hackage.haskell.org/package/streaming-0.2.3.1/docs/Streaming.html#g:3
--

maps :: forall f g m r. (Functor f) => (forall x. f x -> g x) -> Stream f m r -> Stream g m r
maps fn = mapsWithStep (fn . (maps fn <$>))

mapsPost :: forall f g m r. (Functor g) => (forall x. f x -> g x) -> Stream f m r -> Stream g m r
mapsPost fn = mapsWithStep ((mapsPost fn <$>) . fn)

mapsWithStep :: forall f g m r. (forall x. f (Stream f m x) -> g (Stream g m x)) -> Stream f m r -> Stream g m r
mapsWithStep fn = loop 
  where
    loop :: Stream f m r -> Stream g m r
    loop (Return r) = Return r
    loop (Step s) = Step $ fn s
    loop (Effect e) = Effect $ loop <$> e

mapsM :: forall f g m r. (Functor f, Monad m) => (forall x. f x -> m (g x)) -> Stream f m r -> Stream g m r
mapsM fn = mapsMWithStep (fn . (mapsM fn <$>)) 

mapsMPost :: forall g m f r. (Functor g, Monad m) => (forall x. f x -> m (g x)) -> Stream f m r -> Stream g m r
mapsMPost fn = mapsMWithStep (((mapsMPost fn <$>) <$>) . fn) 

mapsMWithDecompose :: forall g m f r. (Functor f, Functor g, Monad m) => (forall x. f x -> m (g x)) -> Stream f m r -> Stream g m r
mapsMWithDecompose fn = decompose . maps (Compose . fn) 

mapsMWithStep :: forall f g m r. (Monad m) => (forall x. f (Stream f m x) -> m (g (Stream g m x))) -> Stream f m r -> Stream g m r
mapsMWithStep fn = loop
  where
    loop (Return r) = Return r
    loop (Step s) = Effect $ fn s >>= return . Step
    loop (Effect e) = Effect $ loop <$> e


groups :: forall m f g r. (Monad m, Functor f, Functor g) => Stream (Sum f g) m r -> Stream (Sum (Stream f m) (Stream g m)) m r
groups (Return r) = Return r
groups (Effect e) = Effect $ groups <$> e
groups (Step (InL f)) = Step $ InL leftStream
  where
    leftStream = Step $ Return . groups <$> f
groups (Step (InR f)) = Step $ InR rightStream
  where
    rightStream = Step $ Return . groups <$> f

takes :: (Monad m, Functor f) => Int -> Stream f m r -> Stream f m ()
takes n str = void $ splitsAt n str

-- ==================== Section: Splitting and joining streams ====================
-- https://hackage.haskell.org/package/streaming-0.2.3.1/docs/Streaming.html#g:5

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

joins :: forall f m r. Functor f => Stream f m (Stream f m r) -> Stream f m r
joins (Return r) = r
joins (Effect e) = Effect $ joins <$> e
joins (Step s) = Step $ joins <$> s

concats :: forall f m r. Functor f => Stream (Stream f m) m r -> Stream f m r
concats (Return r) = Return r
concats (Effect e) = Effect $ concats <$> e
concats (Step (Return r)) = concats r
concats (Step (Effect e)) = Effect $ joins . (concats <$>) <$> e
concats (Step (Step s)) = Step $ joins . (concats <$>) <$> s

-- copy :: forall f m r. Functor f => Stream f m r -> Stream f (Stream f m) r
-- copy (Return r) = Return r
-- -- copy str@(Step _) = Effect $ (copy <$>) $ cp str
-- copy str@(Step _) = undefined
--   where
--     y :: Stream f (Stream f m) (Stream f m (Stream f m r))
--     y = Return $ cp str
-- copy str@(Effect _) = Effect $ (copy <$>) $ cp str

cp :: Functor f => Stream f m r -> Stream f m (Stream f m r)
cp s@(Effect _) = s >> Return s
cp s@(Step _) = s >> Return s
cp s@(Return _) = Return s

-- copyReturn :: Stream f m r -> Stream f m (Stream f m r)
-- copyReturn s@(Return _) = Return s
-- copyReturn (Step s) = do
--   r <- Step $ copyReturn <$> s
--   error "BROKEN"
--   return $ r >> Step s
-- -- return $ r >> Step s
-- copyReturn (Effect e) = do
--   r <- Effect $ copyReturn <$> e
--   error "BROKEN"
--   return $ r >> Effect e

-- store :: Functor f => (Stream f (Stream f m) r -> t) -> Stream f m r -> t
-- store f = f . copy

-- ==================== Section: Inspect ====================

inspect :: Monad m => Stream f m r -> m (Either r (f (Stream f m r)))
inspect (Return r) = pure $ Left r
inspect (Step s) = pure $ Right s
inspect (Effect m) = m >>= inspect

-- ==================== Section: ZIpping ====================
-- https://hackage.haskell.org/package/streaming-0.2.3.1/docs/Streaming.html#g:6

zipsWith :: forall f g h m r. Functor h => (forall x y. f x -> g y -> h (x, y)) -> Stream f m r -> Stream g m r -> Stream h m r
zipsWith fn = zipsWith' (\fn' f g -> uncurry fn' <$> fn f g)

zipsWith' :: forall f g h m r. (forall x y z. (x -> y -> z) -> f x -> g y -> h z) -> Stream f m r -> Stream g m r -> Stream h m r
zipsWith' fn = loop
  where
    loop :: Stream f m r -> Stream g m r -> Stream h m r
    loop (Return r) _ = Return r
    loop _ (Return r) = Return r
    loop (Effect e) s = Effect $ flip loop s <$> e
    loop s (Effect e) = Effect $ loop s <$> e
    loop (Step s1) (Step s2) = Step $ fn loop s1 s2

zips :: (Functor f, Functor g) => Stream f m r -> Stream g m r -> Stream (Compose f g) m r
zips = zipsWith' (\fn s1 s2 -> Compose $ (<$> s2) <$> (fn <$> s1))

unzips :: (Functor f, Functor g) => Stream (Compose f g) m r -> Stream f (Stream g m) r
unzips (Return r) = Return r
unzips (Effect e) = Effect $ Effect $ Return <$> inner
  where
    inner = unzips <$> e
unzips (Step c) = Step x
  where
    x = Effect <$> (Step <$> getCompose (Return <$> inner))
    inner = unzips <$> c


decompose :: forall f m r. (Monad m, Functor f) => Stream (Compose m f) m r -> Stream f m r
decompose (Return r) = Return r
decompose (Effect e) = Effect $ decompose <$> e
decompose (Step s) = Effect $ getCompose s >>= pure . Step . (decompose <$>)

-- ==================== Section: Eliminating ====================
-- https://hackage.haskell.org/package/streaming-0.2.3.1/docs/Streaming.html#g:7

mapsM_ :: (Monad m, Functor f) => (forall x. f x -> m x) -> Stream f m r -> m r
mapsM_ fn = (snd <$>) . collects (((Nothing,) <$>) . fn)

collects :: forall t a f m r. (Monad m, Functor f, Alternative t) => (forall x. f x -> m (t a, x)) -> Stream f m r -> m (t a, r)
collects fn = streamFold foldReturn foldStep foldEffect
  where
    foldReturn r = pure (empty, r)
    foldStep f = appendStep $ fn f
    foldEffect = join 
    appendStep :: m (t a, m (t a, r)) -> m (t a, r)
    appendStep x = do
      (a, res) <- x
      first (a <|>) <$> res

run :: Monad m => Stream m m r -> m r
run = streamFold pure join join

streamFold :: forall f m r b. (Functor f, Monad m) => (r -> b) -> (f b -> b) -> (m b -> b) -> Stream f m r -> b
streamFold foldReturn foldStep foldEffect = loop
  where
    loop :: Stream f m r -> b
    loop (Return r) = foldReturn r
    loop (Step s) = foldStep $ loop <$> s
    loop (Effect e) = foldEffect $ loop <$> e

iterTM :: (Functor f, MonadTrans t, Monad m, Monad (t m)) => (f (t m a) -> t m a) -> Stream f m a -> t m a
iterTM f = streamFold pure f (join . lift)

iterT :: (Functor f, Monad m) => (f (m a) -> m a) -> Stream f m a -> m a
iterT f = streamFold pure f join
