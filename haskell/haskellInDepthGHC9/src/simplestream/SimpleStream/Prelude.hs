{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use =<<" #-}

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

  -- Introducing
  yield,
  each,
  iterate,
  iterateM,
  repeat,
  repeatM,
  replicate,
  replicateM,
  cycle,

  -- Consuming
  stdoutLn,
  stdoutLn',
  readLine,
  prints,
  mapM_,
  drained,
  erase,
  effects,

  -- Transforming
  map,
  mapM,
  for,
  with,
  subst,
  copy,
  duplicate,
  store,
  chain,
  sequence,
  filter,
  filterM,
  take,
  concat,
  read,
  show,

  -- Folding
  fold,
  fold_,
  foldM,
  foldM_,
  all,
  any,
  sum,
  product,
  head,
  last,
  elem, notElem,length,

  toList,
  toList_,
  toListEffect_,
  toListReturn_,

  -- Splitting
  next,
  uncons,

  strWords,
  str10,
  str1,
  collectsOf,

  ) where
{- ORMOLU_ENABLE -}

import Control.Applicative as M
import Control.Monad (join, (>=>))
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Data.Bifunctor
import Data.Functor.Identity
import Data.Kind
import Data.Tuple (swap)
import SimpleStream.Stream as S
import Prelude hiding
  ( all,
    any,
    concat,
    cycle,
    elem,
    filter,
    head,
    iterate,
    last,
    length,
    map,
    mapM,
    mapM_,
    read,
    show,
    notElem,
    product,
    repeat,
    replicate,
    sequence,
    sum,
    take,
  )
import qualified Prelude

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
  where
    loop n = yield n >> loop (iterator n)

iterateM :: forall m a r. Monad m => (a -> m a) -> m a -> StreamOf a m r
iterateM iterator = loop
  where
    loop :: m a -> StreamOf a m r
    loop n = Effect $ do
      v <- n
      return $ Step (v :> loop (iterator v))

repeat :: Monad m => a -> StreamOf a m r
repeat = repeats . toOf

repeatM :: Monad m => m a -> StreamOf a m r
repeatM = repeatsM . (toOf <$>)

replicate :: Monad m => Int -> a -> StreamOf a m ()
replicate n = replicates n . toOf

replicateM :: Monad m => Int -> m a -> StreamOf a m ()
replicateM n = replicatesM n . (toOf <$>)

cycle :: Monad m => StreamOf a m r -> StreamOf a m r
cycle str = loop where loop = str >> loop

toOf :: a -> Of a ()
toOf = (:> ())

-- ==================== Consuming ====================

stdoutLn :: MonadIO m => StreamOf String m r -> m ()
stdoutLn = (const () <$>) . stdoutLn'

stdoutLn' :: MonadIO m => StreamOf String m r -> m r
stdoutLn' = iterT (\(s :> x) -> liftIO (putStrLn s) >> x)

readLine :: (MonadIO m, Read a) => StreamOf a m ()
readLine = Effect $ do
  v <- liftIO $ Prelude.read <$> getLine
  return (Step (v :> readLine))

mapM_ :: Monad m => (a -> m x) -> StreamOf a m r -> m r
mapM_ f =
  effects
    . mapped (\(a :> as) -> (:> as) <$> f a)

prints :: (MonadIO m, Show a) => StreamOf a m r -> m r
prints = mapM_ (liftIO . print)

toList :: Monad m => StreamOf a m r -> m (Of [a] r)
toList = collectsOf @[]

toList_ :: Monad m => StreamOf a m r -> m [a]
toList_ = (fst' <$>) . collectsOf @[]

toListEffect_ :: Monad m => StreamOf a (StreamOf b m) r -> m ([a], [b])
toListEffect_ s = swap . lazily <$> toList (toList_ s)

toListReturn_ :: Monad m => StreamOf a m (StreamOf b m r) -> m ([a], [b])
toListReturn_ s = lazily <$> (toList s >>= _second toList_)

effects :: Monad m => StreamOf a m r -> m r
effects = iterT snd'

drained :: (MonadTrans t, Monad m, Monad (t m)) => t m (StreamOf a m r) -> t m r
drained = join . ((lift . effects) <$>)

erase :: Monad m => StreamOf a m r -> Stream Identity m r
erase = maps (\(_ :> s) -> Identity s)

-- ==================== Transforming ====================

map :: Monad m => (a -> b) -> StreamOf a m r -> StreamOf b m r
map fn = maps $ mapOf fn

mapM :: Monad m => (a -> m b) -> StreamOf a m r -> StreamOf b m r
mapM fn = mapsM $ \(a :> as) -> (:> as) <$> fn a

for :: Monad m => StreamOf a m r -> (a -> StreamOf x m r) -> StreamOf x m r
for v fn = concats $ maps (\(a :> as) -> fn a >> return as) v

with :: (Monad m, Functor f) => StreamOf a m r -> (a -> f x) -> Stream f m r
with str fn = maps (\(a :> as) -> as <$ fn a) str

subst :: (Monad m, Functor f) => (a -> f x) -> StreamOf a m r -> Stream f m r
subst = flip with

copy :: Monad m => StreamOf a m r -> StreamOf a (StreamOf a m) r
copy (Return r) = Return r
copy (Step (a :> str)) = Step (a :> copied)
  where
    copied = Effect $ yield a >> return (copy str)
copy (Effect e) = streamEffectFromEffect_ $ copy <$> e

duplicate :: Monad m => StreamOf a m r -> StreamOf a (StreamOf a m) r
duplicate = copy

store :: Monad m => (StreamOf a (StreamOf a m) r -> t) -> StreamOf a m r -> t
store f = f . copy

chain :: Monad m => (a -> m y) -> StreamOf a m r -> StreamOf a m r
chain fn = mapM (\a -> fn a >> return a)

sequence :: Monad m => StreamOf (m a) m r -> StreamOf a m r
sequence =
  streamFold
    Return
    (\(mx :> rest) -> Effect $ Step . (:> rest) <$> mx)
    Effect

filter :: Monad m => (a -> Bool) -> StreamOf a m r -> StreamOf a m r
filter fn = filterM (pure . fn)

filterM :: Monad m => (a -> m Bool) -> StreamOf a m r -> StreamOf a m r
filterM fn =
  streamFold
    Return
    ( \(a :> as) -> Effect $ do
        x <- fn a
        if x
          then return (Step $ a :> as)
          else return as
    )
    Effect

-- delay :: MonadIO m => Int -> StreamOf a m r -> StreamOf a m r
-- delay n = undefined
--

take :: Monad m => Int -> StreamOf a m r -> StreamOf a m ()
take = takes

concat :: (Foldable f, Monad m) => StreamOf (f a) m r -> StreamOf a m r
concat =
  streamFold
    Return
    (\(f :> xs) -> foldr createStep xs f)
    Effect
  where
    createStep x xs = Step (x :> xs)

read :: (Monad m, Read a) => StreamOf String m r -> StreamOf a m r
read = map Prelude.read

show :: (Monad m, Show a) => StreamOf a m r -> StreamOf String m r
show = map Prelude.show


-- ==================== Splitting ====================

-- ==================== Sum and Compose ====================

-- ==================== Fold ====================

fold :: forall x a m r b. Monad m => (x -> a -> x) -> x -> (x -> b) -> StreamOf a m r -> m (Of b r)
fold reduce x0 finalize = foldM (\x y -> pure $ reduce x y) (pure x0) (pure . finalize)

fold_ :: forall x a m r b. Monad m => (x -> a -> x) -> x -> (x -> b) -> StreamOf a m r -> m b
fold_ reduce mx0 finalize = (fst' <$>) . fold reduce mx0 finalize

foldM :: forall x a m r b. Monad m => (x -> a -> m x) -> m x -> (x -> m b) -> StreamOf a m r -> m (Of b r)
foldM reduce mx0 finalize = loop mx0
  where
    loop :: m x -> StreamOf a m r -> m (Of b r)
    loop mv str = do
      v <- mv
      e <- inspect str
      case e of
        (Left r) -> (:> r) <$> finalize v
        (Right (a :> str')) -> loop (reduce v a) str'

foldM_ :: forall x a m r b. Monad m => (x -> a -> m x) -> m x -> (x -> m b) -> StreamOf a m r -> m b
foldM_ reduce mx0 finalize = (fst' <$>) . foldM reduce mx0 finalize

all :: Monad m => (a -> Bool) -> StreamOf a m r -> m (Of Bool r)
all fn = fold (\x a -> x && fn a) True id

any :: Monad m => (a -> Bool) -> StreamOf a m r -> m (Of Bool r)
any fn = fold (\x a -> x || fn a) False id

sum :: (Monad m, Num a) => StreamOf a m r -> m (Of a r)
sum = fold (+) 0 id

product :: (Monad m, Num a) => StreamOf a m r -> m (Of a r)
product = fold (*) 1 id

head :: Monad m => StreamOf a m r -> m (Of (Maybe a) r)
head = fold (\x a -> x <|> Just a) Nothing id

last :: Monad m => StreamOf a m r -> m (Of (Maybe a) r)
last = fold (\_ a -> Just a) Nothing id

elem :: (Monad m, Eq a) => a -> StreamOf a m r -> m (Of Bool r)
elem a = any (== a)

notElem :: (Monad m, Eq a) => a -> StreamOf a m r -> m (Of Bool r)
notElem a = all (/= a)

length :: Monad m => StreamOf a m r -> m (Of Int r)
length = fold (\v _ -> v + 1) 0 id

-- ==================== Zipping ====================

-- ==================== Splitting ====================

next :: Monad m => StreamOf a m r -> m (Either r (a, StreamOf a m r))
next = inspect >=> pure . (lazily <$>)

uncons :: Monad m => StreamOf a m r -> m (Maybe (a, StreamOf a m r))
uncons = (either (const Nothing) Just <$>) . next

-- ==================== Merge ====================

-- ==================== Maybe ====================

-- ==================== Pair manipulation ====================

-- ==================== Interpolation ====================

-- ==================== Haskell in depth exercise  ====================
--

collectsOf :: (Alternative t, Monad m) => StreamOf a m r -> m (Of (t a) r)
collectsOf =
  (strictly <$>)
    . collects (\(a :> str) -> return (pure a, str))
