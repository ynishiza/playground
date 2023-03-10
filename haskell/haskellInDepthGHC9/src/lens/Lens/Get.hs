{-# LANGUAGE RankNTypes #-}

module Lens.Get
  ( Getter,
    Getting,
    Getting',
    view,
    mkGetter,
  )
where

import Control.Monad.Reader
import Data.Functor.Const
import Data.Functor.Contravariant

type Getter s t a b = forall f. (Contravariant f, Functor f) => (a -> f b) -> s -> f t

type Getting r s a = (a -> Const r a) -> s -> Const r s

type Getting' s a = Getting a s a

view :: MonadReader s m => Getting' s a -> m a
view f = asks (getConst . f Const)

mkGetter :: (s -> a) -> Getter s s a a
mkGetter g fn s = phantom $ fn (g s)
