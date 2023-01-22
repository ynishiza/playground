{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use <&>" #-}

{- ORMOLU_DISABLE -}
module SimpleStream
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

  Of(..),
  StreamOf,
  each,
  ssumM,
  ssum,
  printStream,
  promptOne,
  )
where
{- ORMOLU_ENABLE -}

import Data.Coerce
import Data.Monoid
import Control.Monad
import Control.Monad.IO.Class
import Data.Bifunctor
import Data.Functor.Compose
import Data.Kind
import Fmt
import Text.Read

type Stream :: (Type -> Type) -> (Type -> Type) -> Type -> Type
data Stream f m r where
  Return :: r -> Stream f m r
  Step :: Functor f => f (Stream f m r) -> Stream f m r
  Effect :: Monad m => m (Stream f m r) -> Stream f m r

instance Functor (Stream f m) where
  fmap :: forall a b. (a -> b) -> Stream f m a -> Stream f m b
  fmap f = transform
    where
      transform :: Stream f m a -> Stream f m b
      transform (Return r) = Return $ f r
      transform (Step s) = Step $ transform <$> s
      transform (Effect e) = Effect $ transform <$> e

instance Applicative (Stream f m) where
  pure = Return
  (<*>) = ap

instance Monad (Stream f m) where
  return = pure
  (>>=) :: forall a b. Stream f m a -> (a -> Stream f m b) -> Stream f m b
  m >>= k = transform m
    where
      transform :: Stream f m a -> Stream f m b
      transform (Return r) = k r
      transform (Step s) = Step $ transform <$> s
      transform (Effect e) = Effect $ transform <$> e

type StreamOf a m r = Stream (Of a) m r

type Of :: Type -> Type -> Type
data Of a b where
  (:>) :: a -> b -> Of a b
  deriving (Show)

maps :: forall f g m r. (Functor g) => (forall x. f x -> g x) -> Stream f m r -> Stream g m r
maps fn = transform
  where
    transform :: Stream f m r -> Stream g m r
    transform (Return r) = Return r
    transform (Step s) = Step $ fn $ transform <$> s
    transform (Effect e) = Effect $ transform <$> e

mapOf :: (a -> b) -> StreamOf a m r -> StreamOf b m r
mapOf fn = maps $ first fn 

zipsWith :: forall f g h m r. Functor h => (forall x y z. (x -> y -> z) -> f x -> g y -> h z) -> Stream f m r -> Stream g m r -> Stream h m r
zipsWith fn = transform
  where
    transform :: Stream f m r -> Stream g m r -> Stream h m r
    transform (Return r) _ = Return r
    transform _ (Return r) = Return r
    transform (Effect e) s = Effect $ flip transform s <$> e
    transform s (Effect e) = Effect $ transform s <$> e
    transform (Step s1) (Step s2) = Step $ fn transform s1 s2

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
