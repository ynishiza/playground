{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}

{- ORMOLU_DISABLE -}
module Lens.Prism
  ( 
    Splittable(..),
    APrism,
    APrism',
    toPrism,
    prism,
    prism',
    _Just,
    _Nothing,
    _Left,
    _Right,
    _Show,
    only,
    nearly,
    aside,
    without,
    outside,
    Prefixed(..),
    Suffixed(..),

    below,
    matching,
    matching',
    isn't,

    useSplittable,
    usePrism,
  )
where
{- ORMOLU_ENABLE -}

import Control.Arrow (Arrow (second), (>>>))
import Data.Bifunctor (Bifunctor (bimap))
import Data.Function ((&))
import Data.Functor.Identity
import Data.List qualified as L
import Data.Void (Void, absurd)
import Lens.Lens

type APrism s t a b = Splittable a b a (Identity b) -> Splittable a b s (Identity t)

type APrism' s a = APrism s s a a

data Splittable a b s t where
  Splittable :: (s -> Either t a) -> (b -> t) -> Splittable a b s t

useSplittable :: ((s -> Either t a) -> (b -> t) -> r) -> Splittable a b s t -> r
useSplittable run (Splittable split merge) = run split merge

instance Functor (Splittable a b s) where
  fmap :: (x -> y) -> Splittable a b s x -> Splittable a b s y
  fmap f (Splittable split merge) = Splittable (either (Left . f) Right . split) (f . merge)

instance Profunctor (Splittable a b) where
  dimap :: (u -> s) -> (t -> v) -> Splittable a b s t -> Splittable a b u v
  dimap l r (Splittable split merge) =
    Splittable
      (either (Left . r) Right . split . l)
      (r . merge)

instance ProfunctorChoice (Splittable a b) where
  left' :: Splittable a b s t -> Splittable a b (Either s v) (Either t v)
  left' (Splittable split merge) =
    Splittable
      ( \case
          Left s -> case split s of
            Left t -> Left $ Left t
            Right a -> Right a
          Right v -> Left $ Right v
      )
      (Left . merge)

toPrism :: (s -> Either t a, b -> t) -> Prism s t a b
toPrism (split, merge) k =
  right' k
    & dimap
      split
      ( \case
          Left t -> pure t
          Right fb -> merge <$> fb
      )

fromPrism :: APrism s t a b -> (s -> Either t a, b -> t)
fromPrism lens =
  lens (Splittable Right Identity)
    & useSplittable
      ( \split merge ->
          ( either (Left . runIdentity) Right . split,
            runIdentity . merge
          )
      )

class Prefixed a where
  prefixed :: a -> Prism' a a

class Suffixed a where
  suffixed :: a -> Prism' a a

instance Eq a => Prefixed [a] where
  prefixed as =
    prism'
      ( \l ->
          if as `L.isPrefixOf` l
            then Just (drop (length as) l)
            else Nothing
      )
      id

instance Eq a => Suffixed [a] where
  suffixed as =
    prism'
      ( \l ->
          if as `L.isSuffixOf` l
            then Just (take (length l - length as) l)
            else Nothing
      )
      id

prism :: (s -> Either t a) -> (b -> t) -> Prism s t a b
prism = curry toPrism

prism' :: (s -> Maybe a) -> (b -> s) -> Prism s s a b
prism' split = prism (\s -> maybe (Left s) Right (split s))

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

_Show :: forall a. (Show a, Read a) => Prism' String a
_Show =
  prism
    ( \s -> case reads @a s of
        [(a, _)] -> Right a
        _ -> Left s
    )
    show

_Void :: Prism s s a Void
_Void = prism Left absurd

only :: Eq a => a -> Prism' a a
only a0 = nearly (== a0)

nearly :: (a -> Bool) -> Prism' a a
nearly f = prism (\a -> if f a then Right a else Left a) id

outside :: forall {q} {p} {f} s t a b r. (ProfunctorArrow q, Functor f, ProfunctorRepresentation p) => APrism s t a b -> Optic q f (p t r) (p s r) (p b r) (p a r)
outside lens k =
  k
    & lmap (lmap merge)
    & strong (\t a -> toS (toRep t) . toRep <$> a)
  where
    (split, merge) = fromPrism lens
    toS :: (t -> (Rep p) r) -> (a -> (Rep p) r) -> p s r
    toS pt pa = fromRep $ either pt pa . split

-- Effects

aside :: APrism s t a b -> Prism (e, s) (e, t) (e, a) (e, b)
aside lens =
  prism
    (\(e, s) -> bimap (e,) (e,) $ split s)
    (second merge)
  where
    (split, merge) = fromPrism lens

without :: APrism s t a b -> APrism u v c d -> Prism (Either s u) (Either t v) (Either a c) (Either b d)
without l1 l2 =
  prism
    ( \case
        Left s -> case split s of
          Left t -> Left $ Left t
          Right a -> Right $ Left a
        Right u -> case splitC u of
          Left v -> Left $ Right v
          Right c -> Right $ Right c
    )
    ( \case
        Left b -> Left $ merge b
        Right d -> Right $ mergeD d
    )
  where
    (split, merge) = fromPrism l1
    (splitC, mergeD) = fromPrism l2

below :: Traversable t => APrism' s a -> Prism' (t s) (t a)
below lens =
  prism
    ( \t ->
        traverse split t
          & either (Left . const t) Right
    )
    (merge <$>)
  where
    (split, merge) = fromPrism lens

usePrism :: APrism s t a b -> ((s -> Either t a) -> (b -> t) -> r) -> r
usePrism lens f =
  fromPrism lens
    & uncurry f

isn't :: APrism s t a b -> s -> Bool
isn't lens s = case split s of
  (Left _) -> True
  _ -> False
  where
    (split, _) = fromPrism lens

matching :: APrism s t a b -> s -> Either t a
matching lens = split
  where
    (split, _) = fromPrism lens

matching' :: LensLike (Either a) s t a b -> s -> Either t a
matching' lens s =
  lens Left s
    & either Right Left
