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
import Lens.Lens
import Lens.Index
import Control.Arrow ((>>>))

type Getter s t a b = forall f. (Contravariant f, Functor f) => (a -> f b) -> s -> f t

type Getting r s a = (a -> Const r a) -> s -> Const r s

type Getting' s a = Getting a s a

type IndexedGetting i r s a = Indexed i a (Const r a) -> s -> Const r s

---- Lens

to :: (Profunctor p, Contravariant f) => (s -> a) -> Optic' p f s a
to sa = dimap sa (contramap sa) 

ito :: (Indexable i p, Contravariant f) => (s -> (i, a)) -> Over' p f s a
ito sia pafa s =
  sia s
    & uncurry (indexed pafa)
    & contramap (snd . sia)

like :: (Profunctor p, Functor f, Contravariant f) => a -> Optic' p f s a
like a = dimap (const a) phantom 

ilike :: (Indexable i p, Functor f, Contravariant f) => i -> a -> Over' p f s a
ilike i a pafa _ = indexed pafa i a
  & phantom

-- 
view :: MonadReader s m => Getting' s a -> m a
view lens = views lens id

views :: MonadReader s m => Getting r s a -> (a -> r) -> m r
views lens ar = asks (lens (Const . ar) >>> getConst ) 

iview :: MonadReader s m => IndexedGetting i (i, a) s a -> m (i, a)
iview lens = asks (lens f >>> getConst)
  where f = Indexed $ \i a -> Const (i, a)

iviews :: MonadReader s m => IndexedGetting i r s a -> (i -> a -> r) -> m r
iviews lens ar = asks (lens f >>> getConst)
  where f = Indexed $ \i a -> Const (ar i a)

use :: MonadState s m => Getting a s a -> m a
use lens = uses lens id

uses :: MonadState s m => Getting r s a -> (a -> r) -> m r
uses lens ar = gets (views lens ar)

listening :: MonadWriter s m => Getting' s a -> m b -> m (b, a)
listening lens = listenings lens id 

listenings :: MonadWriter s m => Getting r s a -> (a -> r) -> m b -> m (b, r)
listenings lens ar = listens (views lens ar) 
