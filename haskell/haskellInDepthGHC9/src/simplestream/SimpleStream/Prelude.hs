{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use =<<" #-}
{-# HLINT ignore "Use catMaybes" #-}

{- ORMOLU_DISABLE -}
module SimpleStream.Prelude (
  StreamOf,

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
  nubOrd,
  nubOrdOn,
  mapMaybeM,
  filter,
  filterM,
  delay,
  take,
  takeWhile,
  takeWhileM,
  dropWhile,
  dropWhileM,
  concat,
  scan,
  scanM,
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
  breaks',
  breakWhen,
  breaks,
  break,
  split,
  group,
  groupBy,

  strWords,
  str10,
  str1,
  collectsOf,

  module X,
  ) where
{- ORMOLU_ENABLE -}

import Control.Applicative as M
import Control.Concurrent (threadDelay)
import Control.Monad (join, (>=>))
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Data.Functor.Identity
import Data.Maybe
import Data.Set qualified as D
import Data.Tuple (swap)
import SimpleStream.Of as X
import SimpleStream.Stream as X
import Prelude hiding
  ( all,
    any,
    break,
    concat,
    cycle,
    dropWhile,
    elem,
    filter,
    head,
    iterate,
    last,
    length,
    map,
    mapM,
    mapM_,
    notElem,
    product,
    read,
    repeat,
    replicate,
    sequence,
    show,
    span,
    sum,
    take,
    takeWhile,
  )
import Prelude qualified

strWords :: Monad m => Stream (Of String) m ()
strWords = do
  yield "abc"
  yield "def"
  yield "g"
  yield "h"
  yield "i"

type StreamOf a = Stream (Of a)

str10 :: Stream (Of Int) m ()
str10 = each [1 .. 10]

str1 :: Monad m => Stream (Of Int) m ()
str1 = takes 10 $ repeats (1 :> ())

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

nubOrd :: (Monad m, Ord a) => StreamOf a m r -> StreamOf a m r
nubOrd = nubOrd' D.empty id

nubOrdOn :: (Monad m, Ord b) => (a -> b) -> StreamOf a m r -> StreamOf a m r
nubOrdOn = nubOrd' D.empty

nubOrd' :: (Monad m, Ord b) => D.Set b -> (a -> b) -> StreamOf a m r -> StreamOf a m r
nubOrd' _ _ str@(Return _) = str
nubOrd' seen pick (Effect e) = Effect $ nubOrd' seen pick <$> e
nubOrd' seen pick (Step (a :> as)) =
  if b `D.member` seen
    then nubOrd' seen pick as
    else Step (a :> nubOrd' (b `D.insert` seen) pick as)
  where
    b = pick a

filter :: Monad m => (a -> Bool) -> StreamOf a m r -> StreamOf a m r
filter predicate = filterM (pure . predicate)

filterM :: Monad m => (a -> m Bool) -> StreamOf a m r -> StreamOf a m r
filterM predicate =
  streamFold
    Return
    ( \(a :> as) -> Effect $ do
        x <- predicate a
        if x
          then return (Step $ a :> as)
          else return as
    )
    Effect

mapMaybeM :: Monad m => (a -> m (Maybe b)) -> StreamOf a m r -> StreamOf b m r
mapMaybeM fn = map fromJust . filter isJust . mapM fn

delay :: forall m a r. MonadIO m => Double -> StreamOf a m r -> StreamOf a m r
delay _ str@(Return _) = str
delay n (Effect e) = Effect $ delay n <$> e
delay n (Step (a :> as)) = Effect $ either Return delayIfNext <$> next as
  where
    p = round $ n * 1000 * 1000
    delayIfNext :: (a, StreamOf a m r) -> StreamOf a m r
    delayIfNext (b, bs) = do
      yield a
      liftIO $ threadDelay p
      delay n $ Step (b :> bs)

take :: Monad m => Int -> StreamOf a m r -> StreamOf a m ()
take = takes

takeWhile :: Monad m => (a -> Bool) -> StreamOf a m r -> StreamOf a m ()
takeWhile predicate = takeWhileM (pure . predicate)

takeWhileM :: Monad m => (a -> m Bool) -> StreamOf a m r -> StreamOf a m ()
takeWhileM predicate = breakM ((not <$>) . predicate) >=> return (Return ())

dropWhile :: Monad m => (a -> Bool) -> StreamOf a m r -> StreamOf a m r
dropWhile predicate = dropWhileM (pure . predicate)

dropWhileM :: Monad m => (a -> m Bool) -> StreamOf a m r -> StreamOf a m r
dropWhileM predicate = loop
  where
    loop (Return r) = Return r
    loop (Effect e) = Effect $ loop <$> e
    loop (Step (a :> as)) = Effect $ do
      v <- predicate a
      return $ if v then loop as else Step (a :> as)

concat :: (Foldable f, Monad m) => StreamOf (f a) m r -> StreamOf a m r
concat =
  streamFold
    Return
    (\(f :> xs) -> foldr createStep xs f)
    Effect
  where
    createStep x xs = Step (x :> xs)

scan :: Monad m => (x -> a -> x) -> x -> (x -> b) -> StreamOf a m r -> StreamOf b m r
scan nextValue v0 finalize = scanM nextValue v0 (pure . finalize)

scanM :: Monad m => (x -> a -> x) -> x -> (x -> m b) -> StreamOf a m r -> StreamOf b m r
scanM nextValue v0 finalize = loop v0
  where
    loop _ (Return r) = Return r
    loop v (Effect e) = Effect $ loop v <$> e
    loop v (Step (a :> as)) = Effect $ do
      b <- finalize v'
      return $ Step (b :> loop v' as)
      where
        v' = nextValue v a

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

breaks' :: (Monad m, Functor f) => (forall x. f x -> Bool) -> Stream f m r -> Stream f m (Stream f m r)
breaks' predicate = loop
  where
    loop (Return r) = Return $ Return r
    loop (Effect e) = Effect $ loop <$> e
    loop (Step s) =
      if predicate s
        then Return (Step s)
        else Step $ loop <$> s

breakWhen :: Monad m => (x -> a -> x) -> x -> (x -> b) -> (b -> Bool) -> StreamOf a m r -> StreamOf a m (StreamOf a m r)
breakWhen redc v0 extract predicate = breakWhenM redc v0 extract (pure . predicate)

breakWhenM :: Monad m => (x -> a -> x) -> x -> (x -> b) -> (b -> m Bool) -> StreamOf a m r -> StreamOf a m (StreamOf a m r)
breakWhenM redc v0 extract predicate = loop v0
  where
    loop _ (Return r) = Return $ Return r
    loop accum (Effect e) = Effect $ loop accum <$> e
    loop accum (Step s) = Effect $ do
      p <- predicate (extract accum')
      return $
        if p
          then Return (Step s)
          else Step $ loop accum' <$> s
      where
        accum' = redc accum (fst' s)

break :: Monad m => (a -> Bool) -> StreamOf a m r -> StreamOf a m (StreamOf a m r)
break predicate = breakWhen (\_ a -> predicate a) False id id

breakM :: Monad m => (a -> m Bool) -> StreamOf a m r -> StreamOf a m (StreamOf a m r)
breakM predicate = breakWhenM (\_ a -> predicate a) (pure False) id id

split :: forall m a r. (Monad m, Eq a) => a -> StreamOf a m r -> Stream (StreamOf a m) m r
split a = breaks (== a)

breaks :: forall m a r. Monad m => (a -> Bool) -> StreamOf a m r -> Stream (StreamOf a m) m r
breaks predicate = loop
  where
    loop :: StreamOf a m r -> Stream (StreamOf a m) m r
    loop (Return r) = Return r
    loop (Effect e) = Step $ loop <$> break predicate (Effect e)
    loop (Step s) =
      if predicate (fst' s)
        then loop $ snd' s
        else Step x
      where
        x = Step $ (loop <$>) . break predicate <$> s

group :: forall m a r. (Monad m, Eq a) => StreamOf a m r -> Stream (StreamOf a m) m r
group = groupBy (==)

groupBy :: forall m a r. Monad m => (a -> a -> Bool) -> StreamOf a m r -> Stream (StreamOf a m) m r
groupBy comparer = loop
  where
    b v0 = breakWhen (\_ a -> a) v0 id (not . comparer v0)
    loop :: StreamOf a m r -> Stream (StreamOf a m) m r
    loop (Return r) = Return r
    loop (Effect e) = streamStepFromEffect_ $ loop <$> e
    loop (Step s) = Step $ Step $ (loop <$>) . b (fst' s) <$> s

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
