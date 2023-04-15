{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Collapse lambdas" #-}

{- ORMOLU_DISABLE -}
module Lens.Fold
  ( Fold,
    IndexedFold,

    folded,
    ifoldr,
    repeated,
    replicated,
    iterated,
    taking,
    takingWhile,
    takingWhileBase,
    dropping,
    droppingWhile,
    droppingWhileBase,
    droppingWhileBase2,
    filtered,
    filteredSimple,
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
    findOf,

    -- Indexed
    asIndexed,
    ianyOf,
    elemIndexOf,
    elemIndicesOf,
    ifoldMapOf,
    itoListOf,
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
import Lens.Get
import Lens.Index
import Lens.Lens

asIndexed :: Monoid r => IndexedFold i s a -> IndexedGetting i r s a
asIndexed = id

-- ========== Combinators ==========

folded :: Foldable t => IndexedFold Int (t a) a
folded k = phantom . ifoldr (\i a r -> indexed k i a *> r) (pure ())

folding :: Foldable t => (s -> t a) -> Fold s a
folding f useA = phantom . traverse_ useA . f

ifoldr :: Foldable t => (Int -> a -> r -> r) -> r -> t a -> r
ifoldr combine r0 t = foldr (\a next -> \i -> combine i a (next $ i + 1)) (const r0) t 0

repeated :: Applicative f => LensLike' f a a
repeated = iterated id

iterated :: Applicative f => (a -> a) -> LensLike' f a a
iterated next k = go
  where
    go v = k v *> go (next v)

replicated :: Int -> Fold a a
replicated n = folding (replicate n)

filtered :: (ProfunctorChoice p, Applicative f) => (a -> Bool) -> Optic' p f a a
filtered predicate =
  dimap
    (\a -> if predicate a then Right a else Left a)
    (either pure id)
    . right'

filteredSimple :: Applicative f => (a -> Bool) -> LensLike' f a a
filteredSimple predicate useA a = if predicate a then useA a else pure a

backwards :: Profunctor p => Optic p (Backwards f) s t a b -> Optic p f s t a b
backwards lens = rmap forwards . lens . rmap Backwards

dropping :: Applicative f => LensLike (DroppingWhileCaptured Int a a) s t a a -> Int -> LensLike f s t a a
dropping lens n = droppingWhileBase2 lens (\i _ -> i < n)

droppingWhile :: Applicative f => LensLike (DroppingWhileCaptured Int a a) s t a a -> (a -> Bool) -> LensLike f s t a a
droppingWhile lens predicate = droppingWhileBase2 lens (\_ v -> predicate v)

takingWhile :: forall f a s t. Applicative f => LensLike (TakingWhileCaptured Int a a) s t a a -> (a -> Bool) -> LensLike f s t a a
takingWhile lens predicate = takingWhileBase lens (\_ a -> predicate a)

taking :: Applicative f => LensLike (TakingWhileCaptured Int a a) s t a a -> Int -> LensLike f s t a a
taking lens n = takingWhileBase lens (\i _ -> i < n)

takingWhileBase :: forall f i a s t. (Enum i, Applicative f) => LensLike (TakingWhileCaptured i a a) s t a a -> (i -> a -> Bool) -> LensLike f s t a a
takingWhileBase lens predicate ref =
  lens create
    >>> runTakingWhileCaptured
    >>> traverse ref
    >>> (runCapturedAp <$>)
  where
    create :: a -> TakingWhileCaptured i a a a
    create a = TakingWhileCaptured a $ \(i, isTaking) -> if isTaking && predicate i a then (CCapture () a, True) else (CPure a, False)

droppingWhileBase :: forall f s t a. Applicative f => LensLike (DroppingWhileApplicative f) s t a a -> (Int -> a -> Bool) -> LensLike f s t a a
droppingWhileBase lens predicate useA =
  lens (createDrop useA)
    >>> run
  where
    run :: Applicative f => DroppingWhileApplicative f r -> f r
    run r = fst $ runDroppingWhileApplicative r (0, False)
    createDrop :: (a -> f a) -> a -> DroppingWhileApplicative f a
    createDrop get a = DroppingWhileApplicative $ \(i, isDropping) -> case (isDropping, predicate i a) of
      (False, False) -> (get a, False)
      (False, True) -> (pure a, True)
      (True, True) -> (pure a, True)
      (True, False) -> (get a, False)

droppingWhileBase2 :: forall i f s t a. (Enum i, Applicative f) => LensLike (DroppingWhileCaptured i a a) s t a a -> (i -> a -> Bool) -> LensLike f s t a a
droppingWhileBase2 lens predicate ref =
  lens createDrop
    >>> runDroppingWhileCaptured
    >>> traverse ref
    >>> (runCapturedAp <$>)
  where
    createDrop :: a -> DroppingWhileCaptured i a a a
    createDrop a = DroppingWhileCaptured a $ \(i, isDropping) ->
      if not isDropping || not (predicate i a)
        then (CCapture () a, False)
        else (CPure a, True)

lined :: Fold String String
lined = folding lines

worded :: Fold String String
worded = folding words

-- ========== Exec ==========

has :: Getting Any s a -> s -> Bool
has = notNullOf

runGet :: (a -> r) -> (r -> b) -> Getting r s a -> s -> b
runGet wrapper unwrapper lens = unwrapper . getConst . lens (Const . wrapper)

preview :: Getting (XFirst a) s a -> s -> Maybe a
preview = runGet (XFirst . Just) getXFirst

foldOf :: Getting a s a -> s -> a
foldOf useA = getConst . useA Const

foldMapOf :: Getting a s a -> (a -> r) -> s -> r
foldMapOf useA f = f . foldOf useA

foldrOf :: Getting (Endo r) s a -> (a -> r -> r) -> r -> s -> r
foldrOf getFold f r0 =
  runGet
    (Endo . f)
    (($ r0) . appEndo)
    getFold

foldlOf :: Getting (Dual (Endo r)) s a -> (r -> a -> r) -> r -> s -> r
foldlOf getFold f r0 =
  runGet
    (Dual . Endo . flip f)
    (($ r0) . appEndo . getDual)
    getFold

toNonEmptyOf :: Getting (Endo (N.NonEmpty a)) s a -> a -> s -> N.NonEmpty a
toNonEmptyOf getFold a = runGet (Endo . N.cons) (($ N.singleton a) . appEndo) getFold

toListOf :: Getting (Endo [a]) s a -> s -> [a]
toListOf = runGet (\a -> Endo (a :)) (($ []) . appEndo)

toIndexedList :: (Applicative f) => Getting (Indexing f (Int, a)) s a -> s -> f (Int, a)
toIndexedList = runGet createIndexer (execIndexing 0)
  where
    createIndexer a = Indexing $ \i -> (i + 1, pure (i, a))

allOf :: Getting All s Bool -> s -> Bool
allOf = runGet All getAll

anyOf :: Getting Any s Bool -> s -> Bool
anyOf = runGet Any getAny

elemOf :: Eq a => Getting Any s a -> a -> s -> Bool
elemOf lens a = anyOf (lens . to (== a))

lengthOf :: Getting (Sum Int) s a -> s -> Int
lengthOf = runGet (const (Sum 1)) getSum

nullOf :: Getting Any s a -> s -> Bool
nullOf lens = not . notNullOf lens

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

findOf :: Getting (Maybe a) s a -> (a -> Bool) -> s -> Maybe a
findOf lens predicate = runGet (\a -> if predicate a then Just a else Nothing) id lens

-- ==================== Indexed ====================

runIndexedGet :: (i -> a -> r) -> (r -> m) -> IndexedGetting i r s a -> s -> m
runIndexedGet wrap unwrap lens =
  lens (Indexed $ \i a -> Const $ wrap i a)
    >>> getConst
    >>> unwrap

elemIndexOf :: Eq a => IndexedGetting i (XFirst i) s a -> a -> s -> Maybe i
elemIndexOf lens a0 =
  runIndexedGet
    (\i a -> XFirst $ if a == a0 then Just i else Nothing)
    getXFirst
    lens

elemIndicesOf :: Eq a => IndexedGetting i (Endo [i]) s a -> a -> s -> [i]
elemIndicesOf lens a0 =
  runIndexedGet
    (\i a -> Endo $ \r -> if a == a0 then i : r else r)
    (($ []) . appEndo)
    lens

ifoldMapOf :: IndexedGetting i r s a -> (i -> a -> r) -> s -> r
ifoldMapOf lens f = runIndexedGet f id lens

ianyOf :: IndexedGetting i Any s a -> (i -> a -> Bool) -> s -> Bool
ianyOf lens predicate = runIndexedGet (\i a -> Any $ predicate i a) getAny lens

itoListOf :: IndexedGetting i (Endo [(i, a)]) s a -> s -> [(i, a)]
itoListOf =
  runIndexedGet
    (\i a -> Endo $ \l -> (i, a) : l)
    (($ []) . appEndo)
