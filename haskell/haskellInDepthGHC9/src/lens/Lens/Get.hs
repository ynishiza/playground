{-# LANGUAGE RankNTypes #-}
{-# HLINT ignore "Use curry" #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{- ORMOLU_DISABLE -}
module Lens.Get
  ( Getter,
    Getting,
    Getting',
    IndexedGetting,
    toGetter,
    fromGetter,

    view,
    views,
    views_,
    use,
    uses,
    listening,
    listenings,
    iview, 
    iviews,

    to,
    to_,
    ito,
    ito',
    like,
    ilike,
  )
where
{- ORMOLU_ENABLE -}

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Data.Function ((&))
import Data.Functor.Const
import Data.Functor.Contravariant
import Lens.Index
import Lens.Lens

type Getting r s a = (a -> Const r a) -> s -> Const r s

type Getting' s a = Getting a s a

type IndexedGetting i r s a = Indexed i a (Const r a) -> s -> Const r s

toGetter :: ((a -> r) -> s -> r) -> Getting r s a
toGetter build k s =
  build (getConst . k) s
    & Const

fromGetter :: Getting r s a -> (a -> r) -> s -> r
fromGetter lens k s =
  lens (Const . k) s
    & getConst

---- Lens

to :: (Profunctor p, Contravariant f) => (s -> a) -> Optic' p f s a
to sa = dimap sa (contramap sa)

to_ :: ((a -> r) -> s -> r) -> Getting r s a
to_ = toGetter

ito :: (Indexable i p, Contravariant f) => (s -> (i, a)) -> Over' p f s a
ito f ka s =
  f s
    & uncurry (indexed ka)
    & contramap (snd . f)

ito' :: (Indexable i p, Contravariant f) => (i -> s -> (i, a)) -> p a (f a) -> Indexed i s (f s)
ito' f ka = Indexed $ \i s ->
  uncurry (indexed ka) (f i s)
    & contramap (snd . f i)

like :: (Profunctor p, Functor f, Contravariant f) => a -> Optic' p f s a
like a = dimap (const a) phantom

ilike :: (Indexable i p, Functor f, Contravariant f) => i -> a -> Over' p f s a
ilike i a ka =
  indexed ka i a
    & phantom
    & const

-- Functor effect

view :: MonadReader s m => Getting' s a -> m a
view lens = views lens id

views :: MonadReader s m => Getting r s a -> (a -> r) -> m r
views lens ar = asks (getConst . lens (Const . ar))

views_ :: Getting r s a -> (a -> r) -> s -> r
views_ = fromGetter

iview :: MonadReader s m => IndexedGetting i (i, a) s a -> m (i, a)
iview lens = asks (getConst . lens f)
  where
    f = Indexed $ \i a -> Const (i, a)

iviews :: MonadReader s m => IndexedGetting i r s a -> (i -> a -> r) -> m r
iviews lens ar = asks (getConst . lens f)
  where
    f = Indexed $ \i a -> Const (ar i a)

use :: MonadState s m => Getting a s a -> m a
use lens = uses lens id

uses :: MonadState s m => Getting r s a -> (a -> r) -> m r
uses lens ar = gets (views lens ar)

listening :: MonadWriter s m => Getting' s a -> m b -> m (b, a)
listening lens = listenings lens id

listenings :: MonadWriter s m => Getting r s a -> (a -> r) -> m b -> m (b, r)
listenings lens ar = listens (views lens ar)
