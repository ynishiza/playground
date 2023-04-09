{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

{- ORMOLU_DISABLE -}
module Lens.Set
  ( ASetter,
    ASetter',
    Settable (..),
    Setter,
    IndexedSetter,
    IndexedSetter',
    AIndexedSetter,
    AIndexedSetter',

    sets,
    mapped,
    lifted,
    argument,
    contramapped,

    set,
    set',
    over,
    (+~),
    (-~),
    (?~),

    assign,
    modifying,

    isets,
    iset,
    iover,
  )
where
{- ORMOLU_ENABLE -}

import Control.Arrow ((>>>))
import Control.Monad.State
import Data.Functor.Contravariant
import Data.Functor.Identity
import Lens.Class
import Lens.Index
import Lens.Lens

type Setter s t a b = forall f. Settable f => (a -> f b) -> s -> f t

type ASetter s t a b = (a -> Identity b) -> s -> Identity t

type ASetter' s a = ASetter s s a a

type IndexedSetter i s t a b = forall p f. (Indexable i p, Settable f) => p a (f b) -> s -> f t

type IndexedSetter' i s a = IndexedSetter i s s a a

type AIndexedSetter i s t a b = Indexed i a (Identity b) -> s -> Identity t

type AIndexedSetter' i s a = AIndexedSetter i s s a a

class (Applicative f, Traversable f) => Settable f where
  untainted :: f a -> a
  tainted :: a -> f a

  -- untainedDot f = untainted . f
  -- taintedDot f = tainted . f
  untaintedDot :: Profunctor p => p a (f b) -> p a b
  untaintedDot = rmap untainted
  taintedDot :: Profunctor p => p a b -> p a (f b)
  taintedDot = rmap tainted

instance Settable Identity where
  untainted = runIdentity
  tainted = Identity

-- ==================== Combinators ====================

sets :: (Profunctor p, Profunctor q, Settable f) => (p a b -> q s t) -> Optical p q f s t a b
sets pabqst =
  untaintedDot
    >>> pabqst
    >>> taintedDot

isets :: ((i -> a -> b) -> s -> t) -> IndexedSetter i s t a b
isets f pafb = f (\i a -> untainted (indexed pafb i a))
    >>> tainted

mapped :: Functor f => Setter (f a) (f b) a b
mapped afb =
  ((afb >>> untainted) <$>)
    >>> tainted

contramapped :: Contravariant f => Setter (f b) (f a) a b
contramapped afb =
  contramap (afb >>> untainted)
    >>> tainted

lifted :: Monad m => Setter (m a) (m b) a b
lifted afb =
  (>>= (afb >>> untainted >>> pure))
    >>> tainted

argument :: Profunctor p => Setter (p b r) (p a r) a b
argument afb =
  lmap (afb >>> untainted)
    >>> tainted

-- ==================== Exec ====================

set :: ASetter s t a b -> b -> s -> t
set lens b = over lens (const b)

set' :: ASetter' s a -> a -> s -> s
set' = set

over :: ASetter s t a b -> (a -> b) -> s -> t
over lens fn = runIdentity . lens (Identity . fn)

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

iset :: AIndexedSetter i s t a b -> (i -> b) -> s -> t
iset lens f = iover lens $ \i _ -> f i

iover :: AIndexedSetter i s t a b -> (i -> a -> b) -> s -> t
iover lens f = lens (Indexed $ \i a -> Identity (f i a))
  >>> runIdentity
