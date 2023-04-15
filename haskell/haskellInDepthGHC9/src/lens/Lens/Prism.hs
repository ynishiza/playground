{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

{- ORMOLU_DISABLE -}
module Lens.Prism
  ( prism,
    prism',
    _Just,
    _Nothing,
    _Left,
    _Right,
    only,
    nearly,

    withPrism,
    below,
    matching,
    isn't
  )
where
{- ORMOLU_ENABLE -}

import Data.Function ((&))
import Data.Functor.Identity
import Data.Void (Void, absurd)
import Lens.Lens
import Control.Arrow ((>>>))

type APrism s t a b = Split a b a (Identity b) -> Split a b s (Identity t)

type APrism' s a = APrism s s a a

data Split a b s t where
  Split :: (s -> Either t a) -> (b -> t) -> Split a b s t

instance Functor (Split a b s) where
  fmap :: (x -> y) -> Split a b s x -> Split a b s y
  fmap f (Split split kb) = Split (either (Left . f) Right . split) (f . kb)

instance Profunctor (Split a b) where
  dimap :: (u -> s) -> (t -> v) -> Split a b s t -> Split a b u v
  dimap f r (Split ks kb) =
    Split
      ( \x -> case ks (f x) of
          (Left t) -> Left (r t)
          (Right a) -> Right a
      )
      (r . kb)

instance ProfunctorChoice (Split a b) where
  left' :: Split a b s t -> Split a b (Either s v) (Either t v)
  left' (Split ks kb) =
    Split
      ( \case
          (Left a) -> case ks a of
            Left b -> Left (Left b)
            Right c -> Right c
          (Right v) -> Left (Right v)
      )
      (Left . kb)

-- Combinators

prism :: (s -> Either t a) -> (b -> t) -> Prism s t a b
prism ks kb ka =
  right' ka
    & dimap
      ks
      (either pure (kb <$>))

prism' :: (s -> Maybe a) -> (b -> s) -> Prism s s a b
prism' ks = prism (\s -> maybe (Left s) Right (ks s)) 

_Just :: Prism (Maybe a) (Maybe b) a b
_Just = prism (maybe (Left Nothing) Right) Just

_Nothing :: Prism' (Maybe a) ()
_Nothing = _Nothing' ()

_Nothing' :: b -> Prism (Maybe a) (Maybe a) b c
_Nothing' b = prism' (maybe (Just b) (const Nothing)) (const Nothing)

_Left :: Prism (Either a b) (Either c b) a c
_Left = prism (either Right (Right >>> Left)) Left

_Right :: Prism (Either a b) (Either a c) b c
_Right = prism (either (Left >>> Left) Right) Right

_Void :: Prism s s a Void
_Void = prism Left absurd

only :: Eq a => a -> Prism' a a
only a0 = nearly (== a0)

nearly :: (a -> Bool) -> Prism' a a
nearly f = prism (\a -> if f a then Right a else Left a) id

-- Effects

withPrism :: APrism s t a b -> ((s -> Either t a) -> (b -> t) -> r) -> r
withPrism lens with =
  lens (Split Right Identity)
    & g
  where
    g (Split ks kb) =
      with
        (either (Left . runIdentity) Right . ks)
        (runIdentity . kb)

below :: Traversable t => APrism' s a -> Prism' (t s) (t a)
below lens k = withPrism lens $ \ks kb ->
  k
    & prism (\ts -> either (Left . const ts) Right (traverse ks ts))
        (kb <$>)

isn't :: APrism s t a b -> s -> Bool
isn't lens s =
  withPrism
    lens
    ( \resolve _ -> case resolve s of
        (Left _) -> True
        _ -> False
    )

matching :: APrism s t a b -> s -> Either t a
matching lens s = withPrism lens (\f _ -> f s)
