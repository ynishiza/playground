{-# LANGUAGE RankNTypes #-}

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
    to,
    like,
  )
where

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Data.Functor.Const
import Data.Functor.Contravariant
import Lens.Lens

type Getter s t a b = forall f. (Contravariant f, Functor f) => (a -> f b) -> s -> f t

type Getting r s a = (a -> Const r a) -> s -> Const r s

type Getting' s a = Getting a s a

type IndexedGetting i r s a = Indexed i a (Const r a) -> s -> Const r s

view :: MonadReader s m => Getting' s a -> m a
view getter = views getter id

views :: MonadReader s m => Getting' s a -> (a -> r) -> m r
views getter f = asks (f . getConst . getter Const)

use :: MonadState s m => Getting' s a -> m a
use getter = gets (view getter)

uses :: MonadState s m => Getting' s a -> (a -> r) -> m r
uses getter f = gets (views getter f)

listening :: MonadWriter s m => Getting' s a -> m u -> m (u, a)
listening getter = listens (view getter)

listenings :: MonadWriter s m => Getting' s a -> (a -> r) -> m u -> m (u, r)
listenings getter f = listens (views getter f)

to :: (Profunctor p, Contravariant f) => (s -> a) -> Optic' p f s a
to g = dimap g (contramap g)

like :: (Profunctor p, Contravariant f, Functor f) => a -> Optic' p f s a
like a = dimap (const a) phantom
