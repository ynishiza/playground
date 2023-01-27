{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneKindSignatures #-}

{- ORMOLU_DISABLE -}
module SimpleStream.Of (
  Of(..),
  StreamOf,
  empty,
  effect,
  yield,
  each,
  ssumM,
  ssum,
  printStream,
  promptOne,

  mapOf,
  zipPair,
  withEffect,
  withEffectMap,
  module X,
  ) where
{- ORMOLU_ENABLE -}

import SimpleStream.Stream as X
import Data.Coerce
import Text.Read (readMaybe)
import Data.Kind
import Fmt

type StreamOf a = Stream (Of a)

type Of :: Type -> Type -> Type
data Of a b where
  (:>) :: a -> b -> Of a b
  deriving (Show, Eq)

infixr 1 :>


instance Functor (Of a) where
  fmap f (a :> b) = a :> f b

instance Bifunctor Of where
  bimap f g (a :> b) = f a :> g b

-- ==================== Utils  ====================

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

mapOf :: (a -> b) -> StreamOf a m r -> StreamOf b m r
mapOf fn = maps $ first fn

zipPair :: StreamOf a m r -> StreamOf b m r -> StreamOf (a, b) m r
zipPair = zipsWith (\f (a :> x) (b :> y) -> (a, b) :> f x y)

withEffect :: forall a m r. Monad m => (a -> m ()) -> StreamOf a m r -> StreamOf a m r
withEffect f = mapsM (\v@(a :> _) -> f a >> pure v)

withEffectMap :: forall a b m r. Monad m => (a -> m b) -> StreamOf a m r -> StreamOf b m r
withEffectMap f = mapsM (\(a :> as) -> f a >>= pure . (:> as))

