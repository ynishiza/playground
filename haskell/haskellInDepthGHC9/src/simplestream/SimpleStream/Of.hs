{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE GADTs #-}

module SimpleStream.Of
  (
  Of(..),
  lazily,
  strictly,
  mapOf,
  fst',
  snd',
  _first,
  _second,
  )
where

import Data.Bifunctor
import Data.Kind

type Of :: Type -> Type -> Type
data Of a b where
  (:>) :: !a -> b -> Of a b
  deriving (Show, Eq)

infixr 1 :>

instance Functor (Of a) where
  fmap f (a :> b) = a :> f b

instance Bifunctor Of where
  bimap f g (a :> b) = f a :> g b

instance Monoid a => Applicative (Of a) where
  pure a = mempty :> a
  (a :> f) <*> (b :> x) = a <> b :> f x

lazily :: Of a b -> (a, b)
lazily (a :> b) = (a, b)

strictly :: (a, b) -> Of a b
strictly (a, b) = a :> b

mapOf :: (a -> b) -> Of a x -> Of b x
mapOf f (a :> x) = f a :> x

fst' :: Of a x -> a
fst' (a :> _) = a

snd' :: Of a x -> x
snd' (_ :> x) = x

_first :: Functor f => (a -> f a') -> Of a x -> f (Of a' x)
_first f (a :> x) = (:> x) <$> f a

_second :: Functor f => (x -> f x') -> Of a x -> f (Of a x')
_second f (a :> x) = (a :>) <$> f x

