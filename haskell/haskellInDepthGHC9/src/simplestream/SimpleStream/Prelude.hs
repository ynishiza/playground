{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneKindSignatures #-}

{- ORMOLU_DISABLE -}
module SimpleStream.Prelude (
  Of(..),
  StreamOf,
  lazily,
  strictly,
  fst',
  snd',
  _first,
  _second,
  mapOf,

  yield,
  each,
  iterate,
  iterateM,

  prints,
  map,
  mapM_,
  copy,
  store,

  toList,
  toList_,
  toListEffect_,
  toListReturn_,

  strWords,
  str10,
  str1,
  collectsOf,
  ) where
{- ORMOLU_ENABLE -}

import Control.Applicative as M
import Control.Monad.IO.Class
import Data.Bifunctor
import Data.Kind
import Data.Tuple (swap)
import SimpleStream.Stream as S
import Prelude hiding (map, mapM_, iterate)

strWords :: Monad m => Stream (Of String) m ()
strWords = do
  yield "abc"
  yield "def"
  yield "g"
  yield "h"
  yield "i"

str10 :: Stream (Of Int) m ()
str10 = each [1 .. 10]

str1 :: Monad m => Stream (Of Int) m ()
str1 = takes 10 $ repeats (1 :> ())

type StreamOf a = Stream (Of a)

type Of :: Type -> Type -> Type
data Of a b where
  (:>) :: !a -> b -> Of a b
  deriving (Show, Eq)

infixr 1 :>

instance Functor (Of a) where
  fmap f (a :> b) = a :> f b

instance Bifunctor Of where
  bimap f g (a :> b) = f a :> g b

instance Monoid a => Applicative (Of a) where
  pure a = mempty :> a
  (a :> f) <*> (b :> x) = a <> b :> f x

lazily :: Of a b -> (a, b)
lazily (a :> b) = (a, b)

strictly :: (a, b) -> Of a b
strictly (a, b) = a :> b

mapOf :: (a -> b) -> Of a x -> Of b x
mapOf f (a :> x) = f a :> x

fst' :: Of a x -> a
fst' (a :> _) = a

snd' :: Of a x -> x
snd' (_ :> x) = x

_first :: Functor f => (a -> f a') -> Of a x -> f (Of a' x)
_first f (a :> x) = (:> x) <$> f a

_second :: Functor f => (x -> f x') -> Of a x -> f (Of a x')
_second f (a :> x) = (a :>) <$> f x

-- ==================== Introducing streams of elements  ====================

yield :: a -> StreamOf a m ()
yield a = Step $ a :> empty_

each :: [a] -> StreamOf a m ()
each [] = empty_
each (a : as) = Step $ a :> each as

iterate :: Monad m => (a -> a) -> a -> StreamOf a m r
iterate iterator = loop 
  where loop n = yield n >> loop (iterator n)

iterateM :: forall m a r. Monad m => (a -> m a) -> m a -> StreamOf a m r
iterateM iterator = loop 
  where 
    loop :: m a -> StreamOf a m r
    loop n = Effect $ do
          v <- n
          return $ Step (v :> loop (iterator v))

-- ==================== Consuming ====================

prints :: (MonadIO m, Show a) => StreamOf a m r -> m r
prints (Return r) = pure r
prints (Step (a :> str)) = liftIO (print a) >> prints str
prints (Effect e) = e >>= prints

toList :: Monad m => StreamOf a m r -> m (Of [a] r)
toList = collectsOf @[]

toList_ :: Monad m => StreamOf a m r -> m [a]
toList_ = (fst' <$>) . collectsOf @[]

toListEffect_ :: Monad m => StreamOf a (StreamOf b m) r -> m ([a], [b])
toListEffect_ s = swap . lazily <$> toList (toList_ s)

toListReturn_ :: Monad m => StreamOf a m (StreamOf b m r) -> m ([a], [b])
toListReturn_ s = lazily <$> (toList s >>= _second toList_)

-- ==================== Transforming ====================

map :: Monad m => (a -> b) -> StreamOf a m r -> StreamOf b m r
map fn = maps $ mapOf fn

mapM_ :: Monad m => (a -> m x) -> StreamOf a m r -> m r
mapM_ f str = snd' <$> y
  where
    y = collectsOf @[] x
    x = mapped (\(a :> as) -> (:> as) <$> f a) str

copy :: forall a m r. Monad m => StreamOf a m r -> StreamOf a (StreamOf a m) r
copy (Return r) = Return r
copy (Step (a :> str)) = Step (a :> copied)
  where
    copied = Effect $ yield a >> return (copy str)
copy (Effect e) = streamEffectFromEffect_ $ copy <$> e

store :: Monad m => (StreamOf a (StreamOf a m) r -> t) -> StreamOf a m r -> t
store f = f . copy

-- ==================== Splitting ====================

-- ==================== Sum and Compose ====================

-- ==================== Fold ====================

-- ==================== Zipping ====================

-- ==================== Splitting ====================

-- ==================== Merge ====================

-- ==================== Maybe ====================

-- ==================== Pair manipulation ====================

-- ==================== Interpolation ====================

-- ==================== Haskell in depth exercise  ====================
--

collectsOf :: (Alternative t, Monad m) => StreamOf a m r -> m (Of (t a) r)
collectsOf = (strictly <$>) . collects (\(a :> str) -> return (pure a, str))
