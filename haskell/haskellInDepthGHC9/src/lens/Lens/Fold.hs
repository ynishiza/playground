{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Collapse lambdas" #-}

{- ORMOLU_DISABLE -}
module Lens.Fold
  ( Fold,
    IndexedFold,
    fromFoldMap,
    toFoldMap,

    folded,
    ifoldr,
    folding,
    foldring,
    repeated,
    replicated,
    iterated,
    taking,
    takingWhile,
    takingWhileBase,
    dropping,
    droppingWhile,
    droppingWhileBase,
    filtered,
    filteredSimple,
    backwards,
    lined,
    worded,

    has,
    hasn't,
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
import Data.Coerce (coerce)
import Data.Foldable
import Data.Function ((&))
import Data.Functor.Const
import Data.Functor.Contravariant
import Data.List.NonEmpty qualified as N
import Data.Monoid
import Data.Semigroup (Max (..))
import Lens.Get
import Lens.Index
import Lens.Lens
import Lens.Monoid.Monoid as M

asIndexed :: Monoid r => IndexedFold i s a -> IndexedGetting i r s a
asIndexed = id

toFoldMap :: ((a -> r) -> s -> r) -> Getting r s a
toFoldMap = toGetter

fromFoldMap :: Getting r s a -> (a -> r) -> s -> r
fromFoldMap = fromGetter

-- ========== Combinators ==========

folded :: Foldable t => IndexedFold Int (t a) a
folded k = phantom . ifoldr (\i a r -> indexed k i a *> r) (pure ())

folding :: Foldable t => (s -> t a) -> Fold s a
folding f lens = phantom . traverse_ lens . f

foldring :: (Contravariant f, Applicative f) => ((a -> f a -> f a) -> f a -> s -> f a) -> LensLike f s t a b
foldring build k s =
  build (\a r -> k a *> r) (phantom $ pure ()) s
    & phantom

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
filteredSimple predicate lens a = if predicate a then lens a else pure a

backwards :: Profunctor p => Optic p (Backwards f) s t a b -> Optic p f s t a b
backwards lens = rmap forwards . lens . rmap Backwards

dropping :: Applicative f => LensLike (FreeDrop Int a a) s t a a -> Int -> LensLike f s t a a
dropping lens n = droppingWhileBase lens (\i _ -> i < n)

droppingWhile :: Applicative f => LensLike (FreeDrop Int a a) s t a a -> (a -> Bool) -> LensLike f s t a a
droppingWhile lens predicate = droppingWhileBase lens (\_ v -> predicate v)

takingWhile :: forall f a s t. Applicative f => LensLike (FreeTake Int a a) s t a a -> (a -> Bool) -> LensLike f s t a a
takingWhile lens predicate = takingWhileBase lens (\_ a -> predicate a)

taking :: Applicative f => LensLike (FreeTake Int a a) s t a a -> Int -> LensLike f s t a a
taking lens n = takingWhileBase lens (\i _ -> i < n)

takingWhileBase :: forall f i a s t. (Enum i, Applicative f) => LensLike (FreeTake i a a) s t a a -> (i -> a -> Bool) -> LensLike f s t a a
takingWhileBase lens predicate ref s =
  lens capture s
    & runFreeTake
    & storeTraverses ref
    & (M.fold <$>)
  where
    capture :: a -> FreeTake i a a a
    capture a = FreeTake a $ \(i, isTaking) -> if isTaking && predicate i a then (FStore a, True) else (FPure a, False)

droppingWhileBase :: forall i f s t a. (Enum i, Applicative f) => LensLike (FreeDrop i a a) s t a a -> (i -> a -> Bool) -> LensLike f s t a a
droppingWhileBase lens predicate ref s =
  lens capture s
    & ruunFreeDrop
    & storeTraverses ref
    & (M.fold <$>)
  where
    capture :: a -> FreeDrop i a a a
    capture a = FreeDrop a $ \(i, isDropping) ->
      if not isDropping || not (predicate i a)
        then (FStore a, False)
        else (FPure a, True)

lined :: Fold String String
lined = folding lines

worded :: Fold String String
worded = folding words

-- ========== Exec ==========

has :: Getting Any s a -> s -> Bool
has = notNullOf

hasn't :: Getting Any s a -> s -> Bool
hasn't = nullOf

runGet :: (a -> r) -> (r -> b) -> Getting r s a -> s -> b
runGet wrapper unwrapper lens s =
  lens (Const . wrapper) s
    & getConst
    & unwrapper

preview :: Getting (XFirst a) s a -> s -> Maybe a
preview = runGet (XFirst . Just) getXFirst

foldOf :: Getting a s a -> s -> a
foldOf lens = foldMapOf lens id

foldMapOf :: Getting r s a -> (a -> r) -> s -> r
foldMapOf = fromFoldMap

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
runIndexedGet wrap unwrap lens s =
  lens (Indexed $ \i a -> coerce $ wrap i a) s
    & coerce
    & unwrap

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
