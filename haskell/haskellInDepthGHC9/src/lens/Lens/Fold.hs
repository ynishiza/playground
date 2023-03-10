{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Lens.Fold
  ( Fold,
    IndexedFold,
    folded,
    preview,
    foldOf,
    toListOf,
    allOf,
    anyOf,
    maxOf,
    elemOf,
    lengthOf,
    nullOf,
    maxOf',
  )
where

import Data.Functor.Const
import Data.Functor.Contravariant
import Data.Monoid
import Data.Semigroup (Max (..))
import Lens.Class
import Lens.Get

type Fold s a = forall f. (Contravariant f, Applicative f) => (a -> f a) -> s -> f s

type IndexedFold i s a = forall f p. (Indexable i p, Contravariant f, Applicative f) => p a (f a) -> s -> f s

folded :: forall t a. Foldable t => IndexedFold Int (t a) a
folded getA = phantom . snd . foldr foldA (1 :: Int, pure ())
  where
    foldA a (i, ys) = (i + 1, phantom $ const <$> y <*> ys)
      where
        y = indexed getA i a

preview :: Getting (First a) s a -> s -> Maybe a
preview = runGet (First . Just) getFirst

foldOf :: Getting a s a -> s -> a
foldOf getA = getConst . getA Const

toListOf :: Getting (Endo [a]) s a -> s -> [a]
toListOf = runGet (\a -> Endo (a :)) (($ []) . appEndo)

allOf :: Getting All s Bool -> s -> Bool
allOf = runGet All getAll

anyOf :: Getting Any s Bool -> s -> Bool
anyOf = runGet Any getAny

elemOf :: Eq a => Getting Any s a -> a -> s -> Bool
elemOf getA a = anyOf (getA . mkGetter (== a))

lengthOf :: Getting (Sum Int) s a -> s -> Int
lengthOf = runGet (const (Sum 1)) getSum

nullOf :: Getting Any s a -> s -> Bool
nullOf = runGet (const (Any True)) (not . getAny)

maxOf :: (Bounded a, Ord a) => Getting (Max a) s a -> s -> a
maxOf = runGet Max getMax

maxOf' :: Ord a => Getting (Endo (Maybe a)) s a -> s -> Maybe a
maxOf' = runGet (Endo . mx) (($ Nothing) . appEndo)
  where
    mx v (Just w) = Just (max v w)
    mx v Nothing = Just v

runGet :: Monoid r => (a -> r) -> (r -> b) -> Getting r s a -> s -> b
runGet wrapper unwrapper getA = unwrapper . getConst . getA (Const . wrapper)
