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
  effect,
  yield,
  maps, 
  mapsM,
  mapOf,
  zipsWith,
  zipPair,
  zipCompose,
  decompose,
  withEffect,
  withEffectMap,
  splitsAt,
  ChunkedStream,
  chunks,
  joins,

  Of(..),
  StreamOf,
  each,
  ssumM,
  ssum,
  printStream,
  promptOne,
  module X,
  )
where
{- ORMOLU_ENABLE -}

import Control.Exception
import Control.Monad as X
import Control.Monad.IO.Class as X
import Control.Monad.Trans.Maybe as X
import Control.Monad.Free.Class
import Data.Bifunctor as X
import Data.Coerce
import Data.Functor.Compose as X
import Data.Kind
import Data.Monoid as X
import Fmt
import Text.Read (readMaybe)

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

type StreamOf a = Stream (Of a)

type Of :: Type -> Type -> Type
data Of a b where
  (:>) :: a -> b -> Of a b
  deriving (Show, Eq)

infixr 1 :>

maps :: forall f g m r. (Functor g) => (forall x. f x -> g x) -> Stream f m r -> Stream g m r
maps fn = loop
  where
    loop :: Stream f m r -> Stream g m r
    loop (Return r) = Return r
    loop (Step s) = Step $ fn $ loop <$> s
    loop (Effect e) = Effect $ loop <$> e

mapOf :: (a -> b) -> StreamOf a m r -> StreamOf b m r
mapOf fn = maps $ first fn

zipsWith :: forall f g h m r. Functor h => (forall x y z. (x -> y -> z) -> f x -> g y -> h z) -> Stream f m r -> Stream g m r -> Stream h m r
zipsWith fn = loop
  where
    loop :: Stream f m r -> Stream g m r -> Stream h m r
    loop (Return r) _ = Return r
    loop _ (Return r) = Return r
    loop (Effect e) s = Effect $ flip loop s <$> e
    loop s (Effect e) = Effect $ loop s <$> e
    loop (Step s1) (Step s2) = Step $ fn loop s1 s2

zipPair :: StreamOf a m r -> StreamOf b m r -> StreamOf (a, b) m r
zipPair = zipsWith (\f (a :> x) (b :> y) -> (a, b) :> f x y)

zipCompose :: (Functor f, Functor g) => Stream f m r -> Stream g m r -> Stream (Compose f g) m r
zipCompose = zipsWith (\fn s1 s2 -> Compose $ (<$> s2) <$> (fn <$> s1))

decompose :: forall f m r. (Monad m, Functor f) => Stream (Compose m f) m r -> Stream f m r
decompose (Return r) = Return r
decompose (Effect e) = Effect $ decompose <$> e
decompose (Step s) = Effect $ getCompose s >>= pure . Step . (decompose <$>)

mapsM :: forall f g m r. (Functor g, Monad m) => (forall x. f x -> m (g x)) -> Stream f m r -> Stream g m r
mapsM fn = decompose . maps (Compose . fn)

withEffect :: forall a m r. Monad m => (a -> m ()) -> StreamOf a m r -> StreamOf a m r
withEffect f = mapsM (\v@(a :> _) -> f a >> pure v)

withEffectMap :: forall a b m r. Monad m => (a -> m b) -> StreamOf a m r -> StreamOf b m r
withEffectMap f = mapsM (\(a :> as) -> f a >>= pure . (:> as))

splitsAt :: (Functor f, Monad m) => Int -> Stream f m r -> Stream f m (Stream f m r)
splitsAt n str
  | n > 0 = case str of
      (Return _) -> Return str
      (Effect e) -> Effect $ splitsAt n <$> e
      (Step s) -> Step $ splitsAt (n - 1) <$> s
  | otherwise = Return str

type ChunkedStream f m = Stream (Stream f m) m

chunks :: forall f m r. (Functor f, Monad m) => Int -> Stream f m r -> ChunkedStream f m r
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

copy :: forall f m r. Stream f m r -> Stream f (Stream f m) r
copy (Return r) = Return r
copy (Step s) = Step $ copy <$> s
copy (Effect e) = undefined

-- ==================== Of a ====================
--
instance Functor (Of a) where
  fmap f (a :> b) = a :> f b

instance Bifunctor Of where
  bimap f g (a :> b) = f a :> g b

empty :: Stream f m ()
empty = Return ()

effect :: Monad m => m r -> Stream f m r
effect e = Effect $ Return <$> e

yield :: a -> StreamOf a m ()
yield a = Step $ a :> empty

each :: [a] -> StreamOf a m ()
each [] = empty
each (a : as) = Step $ a :> each as

ssumM :: (Monad m, Monoid a) => StreamOf a m r -> m (Of a r)
ssumM (Return r) = pure $ mempty :> r
ssumM (Step (a :> str)) = first (a <>) <$> ssumM str
ssumM (Effect e) = e >>= ssumM

ssum :: forall a m r. (Monad m, Num a) => StreamOf a m r -> m (Of a r)
ssum = (coerce <$>) . ssumM . mapOf Sum

printStream :: (MonadIO m, Show a, Show r) => StreamOf a m r -> m ()
printStream (Return r) = liftIO $ fmtLn $ "Return:" +|| r ||+ ""
printStream (Step (a :> str)) = liftIO (fmtLn $ "Item:" +|| a ||+ "") >> printStream str
printStream (Effect e) = e >>= printStream

promptOne :: forall a m. (MonadIO m, Read a) => StreamOf a m ()
promptOne = Effect $ do
  liftIO $ fmtLn "Enter number"
  input <- liftIO getLine
  case readMaybe input of
    (Just a) -> pure $ yield a
    Nothing -> liftIO (fmtLn $ "Failed at parse input" +|| input ||+ "") >> pure empty

