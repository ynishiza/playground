{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

{- ORMOLU_DISABLE -}
module Lens.Set
  ( ASetter,
    ASetter',
    Settable (..),

    set',
    sets,
    mapped,
    lifted,
    argument,
    contramapped,

    set,
    over,
    (+~),
    (-~),
    (?~),

    assign,
    modifying,
  )
where
{- ORMOLU_ENABLE -}

import Control.Monad.State
import Data.Functor.Contravariant
import Data.Functor.Identity
import Lens.Class
import Lens.Lens

type Setter s t a b = forall f. Settable f => (a -> f b) -> s -> f t

type ASetter s t a b = (a -> Identity b) -> s -> Identity t

type ASetter' s a = ASetter s s a a

class (Applicative f, Traversable f) => Settable f where
  untainted :: f a -> a
  tainted :: a -> f a

  -- untainedDot f = untainted . f
  -- taintedDot f = tainted . f
  untaintedDot :: Profunctor p => p a (f b) -> p a b
  taintedDot :: Profunctor p => p a b -> p a (f b)

instance Settable Identity where
  untainted = runIdentity
  tainted = Identity
  untaintedDot = rmap runIdentity
  taintedDot = rmap Identity

-- ==================== Combinators ====================

sets :: (Profunctor p, Profunctor q, Settable f) => (p a b -> q s t) -> Optical p q f s t a b
sets fn = taintedDot . fn . untaintedDot

mapped :: Functor f => Setter (f a) (f b) a b
mapped ref = tainted . ((untainted . ref) <$>)

lifted :: Monad m => Setter (m a) (m b) a b
lifted ref = tainted . (>>= pure . untainted . ref)

argument :: Profunctor p => Setter (p b r) (p a r) a b
argument ref = tainted . lmap (untainted . ref)

contramapped :: Contravariant f => Setter (f b) (f a) a b
contramapped ref = tainted . contramap (untainted . ref)

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
