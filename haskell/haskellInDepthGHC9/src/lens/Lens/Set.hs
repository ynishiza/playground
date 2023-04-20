{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}

{- ORMOLU_DISABLE -}
module Lens.Set
  ( ASetter,
    ASetter',
    Settable (..),
    Setter,
    IndexPreservingSetter,
    IndexedSetter,
    IndexedSetter',
    AIndexedSetter,
    AIndexedSetter',
    fromSetter,
    toSetter,

    sets,
    sets_,
    mapped,
    lifted,
    argument,
    contramapped,
    cloneSetter,

    set,
    set',
    over,
    (+~),
    (-~),
    (?~),

    assign,
    modifying,
    scribe,
    passing,
    censoring,

    isets,
    iset,
    iover,
  )
where
{- ORMOLU_ENABLE -}

import Control.Arrow (Arrow (second), (>>>))
import Control.Monad.State
import Control.Monad.Writer.Class
import Data.Function ((&))
import Data.Functor.Contravariant
import Data.Functor.Identity
import Lens.Class
import Lens.Index
import Lens.Lens

type ASetter s t a b = (a -> Identity b) -> s -> Identity t

type ASetter' s a = ASetter s s a a

type AIndexedSetter i s t a b = Indexed i a (Identity b) -> s -> Identity t

type AIndexedSetter' i s a = AIndexedSetter i s s a a

toSetter :: ((a -> b) -> s -> t) -> Setter s t a b
toSetter build k s =
  build (untainted . k) s
    & tainted

fromSetter :: ASetter s t a b -> (a -> b) -> s -> t
fromSetter lens k s =
  lens (tainted . k) s
    & untainted

-- ==================== Combinators ====================

sets :: (Profunctor p, Profunctor q, Settable f) => (p a b -> q s t) -> Optical p q f s t a b
sets lensBase kp =
  rmap untainted kp
    & lensBase
    & rmap tainted

sets_ :: ((a -> b) -> s -> t) -> Setter s t a b
sets_ = toSetter

setting :: ((a -> b) -> s -> t) -> IndexPreservingSetter s t a b
setting f p =
  rmap untainted p
    & strong (\a b -> undefined)
    & undefined

mapped :: Functor f => Setter (f a) (f b) a b
mapped ka x =
  untaintedDot ka <$> x
    & tainted

lifted :: Monad m => Setter (m a) (m b) a b
lifted ka x =
  x
    >>= (pure . untainted . ka)
    & tainted

contramapped :: Contravariant f => Setter (f a) (f b) b a
contramapped ka x =
  contramap (untainted . ka) x
    & tainted

argument :: (Profunctor p) => Setter (p a c) (p b c) b a
argument ka f =
  lmap (untainted . ka) f
    & tainted

isets :: ((i -> a -> b) -> s -> t) -> IndexedSetter i s t a b
isets lensBase ka x =
  lensBase (\i a -> untainted (indexed ka i a)) x
    & tainted

cloneSetter :: ASetter s t a b -> Setter s t a b
cloneSetter lens ka =
  (Identity . untainted . ka)
    & lens
    & (>>> (tainted . runIdentity))

-- ==================== Functor effect ====================

set :: ASetter s t a b -> b -> s -> t
set lens b = over lens (const b)

set' :: ASetter s t a a -> a -> s -> t
set' = set

over :: ASetter s t a b -> (a -> b) -> s -> t
over = fromSetter

(+~) :: Num a => ASetter s t a a -> a -> s -> t
lens +~ v = over lens (+ v)

(-~) :: Num a => ASetter s t a a -> a -> s -> t
lens -~ v = over lens (subtract v)

(?~) :: ASetter s t a (Maybe b) -> b -> s -> t
lens ?~ v = set lens (Just v)

assign :: MonadState s m => ASetter s s a b -> b -> m ()
assign lens b = modify (set lens b)

modifying :: MonadState s m => ASetter s s a b -> (a -> b) -> m ()
modifying lens f = modify (over lens f)

scribe :: (MonadWriter w m, Monoid s) => ASetter s w a a -> a -> m ()
scribe lens a =
  set lens a mempty
    & tell

passing :: MonadWriter w m => ASetter w w u v -> m (a, u -> v) -> m a
passing lens =
  (>>= (return . second (over lens)))
    >>> pass

censoring :: MonadWriter w m => (u -> v) -> ASetter w w u v -> m a -> m a
censoring f lens =
  ((,f) <$>)
    >>> passing lens

iset :: AIndexedSetter i s t a b -> (i -> b) -> s -> t
iset lens f = iover lens $ \i _ -> f i

iover :: AIndexedSetter i s t a b -> (i -> a -> b) -> s -> t
iover lens f =
  lens (Indexed $ \i a -> Identity (f i a))
    >>> runIdentity
