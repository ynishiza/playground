{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use curry" #-}

{- ORMOLU_DISABLE -}
module Lens.Get
  ( Getter,
    Getting,
    Getting',
    IndexedGetting,
    view,
    views,
    use,
    uses,
    listening,
    listenings,
    iview, 
    iviews,

    to,
    ito,
    like,
  )
where
{- ORMOLU_ENABLE -}

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Data.Function ((&))
import Data.Functor.Const
import Data.Functor.Contravariant
import Lens.Lens

type Getter s t a b = forall f. (Contravariant f, Functor f) => (a -> f b) -> s -> f t

type Getting r s a = (a -> Const r a) -> s -> Const r s

type Getting' s a = Getting a s a

type IndexedGetting i r s a = Indexed i a (Const r a) -> s -> Const r s

view :: MonadReader s m => Getting' s a -> m a
view lens = views lens id

views :: MonadReader s m => Getting' s a -> (a -> r) -> m r
views lens f = asks (f . getConst . lens Const)

use :: MonadState s m => Getting' s a -> m a
use lens = gets (view lens)

uses :: MonadState s m => Getting' s a -> (a -> r) -> m r
uses lens f = gets (views lens f)

listening :: MonadWriter s m => Getting' s a -> m u -> m (u, a)
listening lens = listens (view lens)

listenings :: MonadWriter s m => Getting' s a -> (a -> r) -> m u -> m (u, r)
listenings lens f = listens (views lens f)

iview :: MonadReader s m => IndexedGetting i (i, a) s a -> m (i, a)
iview lens = iviews lens (curry id)

iviews :: MonadReader s m => IndexedGetting i r s a -> (i -> a -> r) -> m r
iviews lens mapping = asks (getConst . lens (Indexed $ \i a -> Const $ mapping i a))

to :: (Profunctor p, Contravariant f) => (s -> a) -> Optic' p f s a
to g = dimap g (contramap g)

ito :: (Indexable i p, Contravariant f) => (s -> (i, a)) -> Over' p f s a
ito g ref s =
  indexed ref i a
    & contramap (snd . g)
  where
    (i, a) = g s

like :: (Profunctor p, Contravariant f, Functor f) => a -> Optic' p f s a
like a = dimap (const a) phantom
