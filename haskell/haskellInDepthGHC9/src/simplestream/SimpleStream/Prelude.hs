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
  yield,
  each,
  ssumM,
  printStream,
  promptOne,

  zipPair,
  withEffect,
  withEffectMap,
  prints,
  prints2,
  ssum,
  map,
  mapM_,
  copy,
  store,
  collectsOf,
  toList,
  toList_,

  strWords,
  str10,
  str1,
  ) where
{- ORMOLU_ENABLE -}

import Control.Applicative as M
import Control.Monad.IO.Class
import Data.Bifunctor
import Data.Coerce
import Data.Kind
import Data.Monoid qualified as M
import Fmt
import SimpleStream.Stream as S
import Text.Read (readMaybe)
import Prelude hiding (map, mapM_)

strWords :: Stream (Of String) m ()
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

yield :: a -> StreamOf a m ()
yield a = Step $ a :> empty_

each :: [a] -> StreamOf a m ()
each [] = empty_
each (a : as) = Step $ a :> each as

prints :: (MonadIO m, Show a) => StreamOf a m r -> m r
prints (Return r) = pure r
prints (Step (a :> str)) = liftIO (print a) >> prints str
prints (Effect e) = e >>= prints

prints2 :: (MonadIO m, Show a) => StreamOf a m r -> m r
prints2 str = prints (cp str) >>= prints

map :: (a -> b) -> StreamOf a m r -> StreamOf b m r
map fn = maps $ strictly . first fn . lazily

mapM_ :: Monad m => (a -> m x) -> StreamOf a m r -> m r
mapM_ f str = snd . lazily <$> y
  where
    y = collectsOf @[] x
    x = mapsM (\(a :> as) -> (:> as) <$> f a) str

copy :: forall a m r. Monad m => StreamOf a m r -> StreamOf a (StreamOf a m) r
copy (Return r) = Return r
copy (Step (a :> str)) = Step (a :> copied)
  where
    copied = Effect $ yield a >> return (copy str)
copy (Effect e) = Effect x
  where
    x = Effect $ Return . copy <$> e

store :: Monad m => (StreamOf a (StreamOf a m) r -> t) -> StreamOf a m r -> t
store f = f . copy

toList :: Monad m => StreamOf a m r -> m (Of [a] r)
toList = collectsOf @[]

toList_ :: Monad m => StreamOf a m r -> m [a]
toList_ = (fst . lazily <$>) . collectsOf @[]

-- ==================== Haskell in depth exercise  ====================
--

ssumM :: (Monad m, Monoid a) => StreamOf a m r -> m (Of a r)
ssumM (Return r) = pure $ mempty :> r
ssumM (Step (a :> str)) = first (a <>) <$> ssumM str
ssumM (Effect e) = e >>= ssumM

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
    Nothing -> liftIO (fmtLn $ "Failed at parse input" +|| input ||+ "") >> pure empty_

collectsOf :: (Alternative t, Monad m) => StreamOf a m r -> m (Of (t a) r)
collectsOf = (strictly <$>) . collects (\(a :> str) -> return (pure a, str))

zipPair :: StreamOf a m r -> StreamOf b m r -> StreamOf (a, b) m r
zipPair = zipsWith' (\f (a :> x) (b :> y) -> (a, b) :> f x y)

withEffect :: forall a m r. Monad m => (a -> m ()) -> StreamOf a m r -> StreamOf a m r
withEffect f = mapsM (\v@(a :> _) -> f a >> pure v)

withEffectMap :: forall a b m r. Monad m => (a -> m b) -> StreamOf a m r -> StreamOf b m r
withEffectMap f = mapsM (\(a :> as) -> f a >>= pure . (:> as))

ssum :: forall a m r. (Monad m, Num a) => StreamOf a m r -> m (Of a r)
ssum = (coerce <$>) . ssumM . map M.Sum
