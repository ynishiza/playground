{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

{- ORMOLU_DISABLE -}
module Lens.Fold
  ( Fold,
    IndexedFold,

    folded,
    repeated,
    replicated,
    iterated,
    taking,
    takingWhile,
    filtered,
    backwards,
    lined,
    worded,

    has,
    preview,
    foldOf,
    foldMapOf,
    foldrOf,
    foldlOf,
    toNonEmptyOf,
    toIndexedList,
    toListOf,
    allOf,
    anyOf,
    maxOf,
    elemOf,
    lengthOf,
    nullOf,
    notNullOf,
    maxOf',
    firstOf,
    lastOf,
  )
where
{- ORMOLU_ENABLE -}

import Control.Applicative.Backwards
import Control.Arrow ((>>>))
import Data.Foldable
import Data.Functor.Const
import Data.Functor.Contravariant
import Data.List.NonEmpty qualified as N
import Data.Monoid
import Data.Semigroup (Max (..))
import Lens.Class
import Lens.Get

type Fold s a = forall f. (Contravariant f, Applicative f) => (a -> f a) -> s -> f s

type IndexedFold i s a = forall f p. (Indexable i p, Contravariant f, Applicative f) => p a (f a) -> s -> f s

-- ========== Combinators ==========

folded :: Foldable t => IndexedFold Int (t a) a
folded getA = phantom . snd . foldr foldA (1 :: Int, pure ())
  where
    foldA a (i, ys) = (i + 1, phantom $ const <$> y <*> ys)
      where
        y = indexed getA i a

folding :: Foldable t => (s -> t a) -> Fold s a
folding f getA = phantom . traverse_ getA . f

repeated :: Fold a a
repeated = iterated id

iterated :: (a -> a) -> Fold a a
iterated next getA = go
  where
    go v = getA v *> go (next v)

replicated :: Int -> Fold a a
replicated n = folding (replicate n)

filtered :: (a -> Bool) -> Fold a a
filtered predicate getA a = if predicate a then getA a else phantom $ pure ()

backwards :: Profunctor p => Optic p (Backwards f) s t a b -> Optic p f s t a b
backwards ref get = a
  where
    a = rmap forwards $ ref $ rmap Backwards get

taking :: forall s a. Fold s a -> Int -> Fold s a
taking fld n = takingWhileBase fld $ \i _ -> i < n

takingWhile :: forall s a. Fold s a -> (a -> Bool) -> Fold s a
takingWhile fld predicate = takingWhileBase fld (const predicate)

takingWhileBase :: forall s a. Fold s a -> (Int -> a -> Bool) -> Fold s a
takingWhileBase fld predicate getA =
  fld (Const . createTakeWhile (phantom . getA))
    >>> getConst
    >>> run
    >>> phantom
  where
    run :: Applicative f => TakeWhileR (Ap f ()) -> f ()
    run r = maybe (pure ()) getAp $ runTakeWhileR r 0
    createTakeWhile :: (a -> f ()) -> a -> TakeWhileR (Ap f ())
    createTakeWhile get a = TakeWhileR $ \i ->
      if predicate i a
        then Just (Ap $ get a)
        else Nothing

lined :: Fold String String
lined = folding lines

worded :: Fold String String
worded = folding words

-- ========== Exec ==========

has :: Getting Any s a -> s -> Bool
has = notNullOf

runGet :: (a -> r) -> (r -> b) -> Getting r s a -> s -> b
runGet wrapper unwrapper getA = unwrapper . getConst . getA (Const . wrapper)

preview :: Getting (XFirst a) s a -> s -> Maybe a
preview = runGet (XFirst . Just) getXFirst

foldOf :: Getting a s a -> s -> a
foldOf getA = getConst . getA Const

foldMapOf :: Getting a s a -> (a -> r) -> s -> r
foldMapOf getA f = f . foldOf getA

foldrOf :: Getting (Endo r) s a -> (a -> r -> r) -> r -> s -> r
foldrOf getFold f r0 = runGet (Endo . f) (($ r0) . appEndo) getFold

foldlOf :: Getting (Dual (Endo r)) s a -> (r -> a -> r) -> r -> s -> r
foldlOf getFold f r0 = runGet (Dual . Endo . flip f) (($ r0) . appEndo . getDual) getFold

toNonEmptyOf :: Getting (Endo (N.NonEmpty a)) s a -> a -> s -> N.NonEmpty a
toNonEmptyOf getFold a = runGet (Endo . N.cons) (($ N.singleton a) . appEndo) getFold

toListOf :: Getting (Endo [a]) s a -> s -> [a]
toListOf = runGet (\a -> Endo (a :)) (($ []) . appEndo)

toIndexedList :: (Applicative f) => Getting (Indexing f (Int, a)) s a -> s -> f (Int, a)
toIndexedList = runGet createIndexer (snd . ($ 0) . runIndexing)
  where
    createIndexer a = Indexing $ \i -> (i + 1, pure (i, a))

allOf :: Getting All s Bool -> s -> Bool
allOf = runGet All getAll

anyOf :: Getting Any s Bool -> s -> Bool
anyOf = runGet Any getAny

elemOf :: Eq a => Getting Any s a -> a -> s -> Bool
elemOf getAny a = anyOf (getAny . to (== a))

lengthOf :: Getting (Sum Int) s a -> s -> Int
lengthOf = runGet (const (Sum 1)) getSum

nullOf :: Getting Any s a -> s -> Bool
nullOf getA = not . notNullOf getA

notNullOf :: Getting Any s a -> s -> Bool
notNullOf = runGet (const (Any True)) getAny

firstOf :: Getting (XFirst a) s a -> s -> Maybe a
firstOf = runGet (XFirst . Just) getXFirst

lastOf :: Getting (Last a) s a -> s -> Maybe a
lastOf = runGet (Last . Just) getLast

maxOf :: (Bounded a, Ord a) => Getting (Max a) s a -> s -> a
maxOf = runGet Max getMax

maxOf' :: Ord a => Getting (Endo (Maybe a)) s a -> s -> Maybe a
maxOf' = runGet (Endo . mx) (($ Nothing) . appEndo)
  where
    mx v (Just w) = Just (max v w)
    mx v Nothing = Just v
