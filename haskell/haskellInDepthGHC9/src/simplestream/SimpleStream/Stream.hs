{-# HLINT ignore "Use <&>" #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# HLINT ignore "Use section" #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UndecidableInstances #-}
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
  streamBuild,
  delays,

  maps, 
  mapsPost,
  mapped,
  mappedPost,
  mapsMWithDecompose,
  -- hoistExposed,
  -- distribute,

  groups,
  takes,
  ChunkedStream,
  splitsAt,
  chunks,
  joins,
  concats,
  intercalates,
  -- cutoff,

  inspect, 

  zipsWith,
  zipsWith',
  zips,
  unzips,
  interleaves,
  separate,
  unseparate,
  decompose,
  expand,
  expandPost,

  mapsM_,
  run,
  iterTM,
  iterT,
  streamFold,
  destroy,
  collects,

  streamStepFromStep,
  streamStepFromStep_,
  streamStepFromEffect,
  streamEffectFromEffect,
  streamEffectFromEffect_,
  streamEffectFromStep,
  )
where
{- ORMOLU_ENABLE -}

import Control.Applicative as X
import Control.Concurrent
import Control.Exception
import Control.Monad as X
import Control.Monad.Free.Class
import Control.Monad.IO.Class as X
import Control.Monad.Trans.Class
import Control.Monad.Writer hiding (Sum)
import Data.Bifunctor as X
import Data.Functor.Compose as X
import Data.Functor.Sum
import Data.Kind
import Fmt

type Stream :: (Type -> Type) -> (Type -> Type) -> Type -> Type
data Stream f m r where
  Return :: r -> Stream f m r
  Step :: f (Stream f m r) -> Stream f m r
  Effect :: m (Stream f m r) -> Stream f m r

instance (Monad m, Functor f) => MonadFree f (Stream f m) where
  wrap = Step

instance (Monad m, Functor f) => Functor (Stream f m) where
  fmap :: forall a b. (a -> b) -> Stream f m a -> Stream f m b
  fmap f = loop
    where
      loop :: Stream f m a -> Stream f m b
      loop (Return r) = Return $ f r
      loop (Step s) = wrap $ loop <$> s
      loop (Effect e) = Effect $ loop <$> e

instance (Monad m, Functor f) => Applicative (Stream f m) where
  pure = Return
  (<*>) = ap

instance (Monad m, Functor f) => Monad (Stream f m) where
  return = pure
  (>>=) :: forall a b. Stream f m a -> (a -> Stream f m b) -> Stream f m b
  m >>= k = loop m
    where
      loop :: Stream f m a -> Stream f m b
      loop (Return r) = k r
      loop (Step s) = wrap $ loop <$> s
      loop (Effect e) = Effect $ loop <$> e

instance MonadTrans (Stream f) where
  lift :: Monad m => m a -> Stream f m a
  lift = effect

instance (MonadIO m, Functor f) => MonadIO (Stream f m) where
  liftIO :: IO a -> Stream f m a
  liftIO io = Effect $ do
    x <- liftIO io
    return $ Return x

instance (Functor f, MonadWriter w m) => MonadWriter w (Stream f m) where
  tell w = Effect $ tell w >> return (Return ())
  pass s = s >>= effect . pass . pure
  listen (Return r) = Return (r, mempty)
  listen (Step s) = Step $ listen <$> s
  listen (Effect e) = Effect $ do
    (s, w) <- listen e
    return $ (,w) <$> s

instance (Monad m, Applicative f) => Alternative (Stream f m) where
  empty = never
  (<|>) :: Stream f m r -> Stream f m r -> Stream f m r
  (<|>) = zipsWith $ \f g -> (,) <$> f <*> g

-- ==================== Section: Constructing a stream  ====================
-- https://hackage.haskell.org/package/streaming-0.2.3.1/docs/Streaming.html#g:2

empty_ :: Stream f m ()
empty_ = Return ()

-- lift for Step
yields :: Functor f => f r -> Stream f m r
yields v = Step $ Return <$> v

-- lift for Effect
effect :: Monad m => m r -> Stream f m r
effect e = Effect $ Return <$> e

replicates :: (Monad m, Functor f) => Int -> f () -> Stream f m ()
replicates n = takes n . repeats

repeats :: (Monad m, Functor f) => f () -> Stream f m ()
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

streamBuild :: (forall b. (r -> b) -> (f b -> b) -> (m b -> b) -> b) -> Stream f m r
streamBuild builder = builder Return Step Effect

-- ==================== Section: transforming a stream  ====================
-- https://hackage.haskell.org/package/streaming-0.2.3.1/docs/Streaming.html#g:3
--

maps :: forall f g m r. (Monad m, Functor f) => (forall x. f x -> g x) -> Stream f m r -> Stream g m r
maps fn = mapsWithStep (fn . (maps fn <$>))

mapsPost :: forall f g m r. (Monad m, Functor g) => (forall x. f x -> g x) -> Stream f m r -> Stream g m r
mapsPost fn = mapsWithStep ((mapsPost fn <$>) . fn)

mapsWithStep :: forall f g m r. Monad m => (forall x. f (Stream f m x) -> g (Stream g m x)) -> Stream f m r -> Stream g m r
mapsWithStep fn = loop
  where
    loop :: Stream f m r -> Stream g m r
    loop (Return r) = Return r
    loop (Step s) = Step $ fn s
    loop (Effect e) = Effect $ loop <$> e

mapped :: forall f g m r. (Functor f, Monad m) => (forall x. f x -> m (g x)) -> Stream f m r -> Stream g m r
mapped fn = mapsMWithStep (fn . (mapped fn <$>))

mappedPost :: forall g m f r. (Functor g, Monad m) => (forall x. f x -> m (g x)) -> Stream f m r -> Stream g m r
mappedPost fn = mapsMWithStep (((mappedPost fn <$>) <$>) . fn)

mapsMWithDecompose :: forall g m f r. (Functor f, Functor g, Monad m) => (forall x. f x -> m (g x)) -> Stream f m r -> Stream g m r
mapsMWithDecompose fn = decompose . maps (Compose . fn)

mapsMWithStep :: forall f g m r. (Monad m) => (forall x. f (Stream f m x) -> m (g (Stream g m x))) -> Stream f m r -> Stream g m r
mapsMWithStep fn = loop
  where
    loop (Return r) = Return r
    loop (Step s) = Effect $ fn s >>= return . Step
    loop (Effect e) = Effect $ loop <$> e

groups :: forall m f g r. (Monad m, Functor f, Functor g) => Stream (Sum f g) m r -> Stream (Sum (Stream f m) (Stream g m)) m r
groups =
  streamFold
    Return
    ( \case
        (InL x) -> streamStepFromStep InL x
        (InR x) -> streamStepFromStep InR x
    )
    Effect

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

joins :: forall f m r. (Monad m, Functor f) => Stream f m (Stream f m r) -> Stream f m r
joins =
  streamFold
    id
    Step
    Effect

concats :: forall f m r. (Monad m, Functor f) => Stream (Stream f m) m r -> Stream f m r
concats =
  streamFold
    Return
    joins
    Effect

intercalates :: (Monad m, MonadTrans t, Monad (t m)) => t m x -> Stream (t m) m r -> t m r
intercalates v =
  streamFold
    return
    ((v >>) =<<)
    (join . lift)

-- ==================== Section: Inspect ====================

inspect :: Monad m => Stream f m r -> m (Either r (f (Stream f m r)))
inspect (Return r) = pure $ Left r
inspect (Step s) = pure $ Right s
inspect (Effect m) = m >>= inspect

-- ==================== Section: ZIpping ====================
-- https://hackage.haskell.org/package/streaming-0.2.3.1/docs/Streaming.html#g:6

zipsWith :: forall f g h m r. (Monad m, Functor h) => (forall x y. f x -> g y -> h (x, y)) -> Stream f m r -> Stream g m r -> Stream h m r
zipsWith fn = zipsWith' (\fn' f g -> uncurry fn' <$> fn f g)

zipsWith' :: forall f g h m r. (Monad m) => (forall x y z. (x -> y -> z) -> f x -> g y -> h z) -> Stream f m r -> Stream g m r -> Stream h m r
zipsWith' fn = loop
  where
    loop :: Stream f m r -> Stream g m r -> Stream h m r
    loop (Return r) _ = Return r
    loop _ (Return r) = Return r
    loop (Effect e) s = Effect $ flip loop s <$> e
    loop s (Effect e) = Effect $ loop s <$> e
    loop (Step s1) (Step s2) = Step $ fn loop s1 s2

zips :: (Monad m, Functor f, Functor g) => Stream f m r -> Stream g m r -> Stream (Compose f g) m r
zips = zipsWith' (\fn s1 s2 -> Compose $ (<$> s2) <$> (fn <$> s1))

unzips :: (Monad m, Functor f, Functor g) => Stream (Compose f g) m r -> Stream f (Stream g m) r
unzips =
  streamFold
    Return
    (\(Compose c) -> Step (Effect . yields <$> c))
    streamEffectFromEffect_

interleaves :: (Monad m, Applicative h) => Stream h m r -> Stream h m r -> Stream h m r
interleaves = zipsWith' (\p f g -> p <$> f <*> g)

separate :: (Functor f, Functor g, Monad m) => Stream (Sum f g) m r -> Stream f (Stream g m) r
separate =
  streamFold
    Return
    ( \case
        (InL s) -> Step s
        (InR s) -> streamEffectFromStep id s
    )
    streamEffectFromEffect_

unseparate :: (Monad m, Functor f, Functor g) => Stream f (Stream g m) r -> Stream (Sum f g) m r
unseparate =
  streamFold
    Return
    (Step . InL)
    (joins . maps InR)

decompose :: (Monad m, Functor f) => Stream (Compose m f) m r -> Stream f m r
decompose =
  streamFold
    Return
    (Effect . (Step <$>) . getCompose)
    Effect

expand :: (Functor f, Monad m) => (forall a b. (g a -> b) -> f a -> h b) -> Stream f m r -> Stream g (Stream h m) r
expand mapf =
  streamFold
    Return
    (Effect . Step . mapf (Return . Step))
    (Effect . effect)

expandPost :: (Functor g, Monad m) => (forall a b. (g a -> b) -> f a -> h b) -> Stream f m r -> Stream g (Stream h m) r
expandPost mapf = loop
  where
    loop (Return r) = Return r
    loop (Effect e) = Effect $ effect $ loop <$> e
    loop (Step s) = Effect $ Step $ mapf (Return . Step . (loop <$>)) s

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

streamFold :: (Monad m, Functor f) => (r -> b) -> (f b -> b) -> (m b -> b) -> Stream f m r -> b
streamFold buildReturn buildStep buildEffect = loop
  where
    loop str = case str of
      (Return r) -> buildReturn r
      (Step s) -> buildStep $ loop <$> s
      (Effect e) -> buildEffect $ loop <$> e

destroy :: (Monad m, Functor f) => Stream f m r -> (f b -> b) -> (m b -> b) -> (r -> b) -> b
destroy s a b c = streamFold c a b s

iterTM :: (Functor f, MonadTrans t, Monad m, Monad (t m)) => (f (t m a) -> t m a) -> Stream f m a -> t m a
iterTM f = streamFold pure f (join . lift)

iterT :: (Functor f, Monad m) => (f (m a) -> m a) -> Stream f m a -> m a
iterT f = streamFold pure f join

streamStepFromStep_ :: Functor f => f (Stream (Stream f m) n r) -> Stream (Stream f m) n r
streamStepFromStep_ = streamStepFromStep id

streamStepFromStep :: Functor f => (Stream f x (Stream str n r) -> str (Stream str n r)) -> f (Stream str n r) -> Stream str n r
streamStepFromStep fn = Step . fn . yields

streamStepFromEffect :: Monad m => (Stream x m (Stream str n r) -> str (Stream str n r)) -> m (Stream str n r) -> Stream str n r
streamStepFromEffect fn = Step . fn . lift

streamEffectFromEffect_ :: (Monad m) => m (Stream f (Stream g m) r) -> Stream f (Stream g m) r
streamEffectFromEffect_ = streamEffectFromEffect id

streamEffectFromEffect :: (Monad m) => (Stream x m (Stream f str r) -> str (Stream f str r)) -> m (Stream f str r) -> Stream f str r
streamEffectFromEffect fn = Effect . fn . lift

streamEffectFromStep :: Functor f => (Stream f x (Stream g str r) -> str (Stream g str r)) -> f (Stream g str r) -> Stream g str r
streamEffectFromStep fn = Effect . fn . yields
