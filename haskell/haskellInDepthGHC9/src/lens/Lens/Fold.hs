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
    droppingWhile,
    filtered,
    filtered',
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

    -- OLD
    takingWhileSimple,
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
import Lens.Lens

type Fold s a = forall f. (Contravariant f, Applicative f) => (a -> f a) -> s -> f s

type IndexedFold i s a = forall f p. (Indexable i p, Contravariant f, Applicative f) => p a (f a) -> s -> f s

asIndexed :: Monoid r => IndexedFold i s a -> IndexedGetting i r s a
asIndexed = id

-- ========== Combinators ==========

folded :: Foldable t => IndexedFold Int (t a) a
folded lens = phantom . ifoldr (\i a r -> indexed lens i a *> r) (pure ())

folding :: Foldable t => (s -> t a) -> Fold s a
folding f useA = phantom . traverse_ useA . f

ifoldr :: Foldable t => (Int -> a -> r -> r) -> r -> t a -> r
ifoldr combine r0 t = foldr (\a next -> \i -> combine i a (next $ i + 1)) (const r0) t 0

repeated :: Applicative f => LensLike' f a a
repeated = iterated id

iterated :: Applicative f => (a -> a) -> LensLike' f a a
iterated next lens = go
  where
    go v = lens v *> go (next v)

replicated :: Int -> Fold a a
replicated n = folding (replicate n)

filtered' :: (Choice p, Applicative f) => (a -> Bool) -> Optic' p f a a
filtered' predicate =
  dimap
    (\a -> if predicate a then Right a else Left a)
    (either pure id)
    . right'

filtered :: Applicative f => (a -> Bool) -> LensLike' f a a
filtered predicate useA a = if predicate a then useA a else pure a

backwards :: Profunctor p => Optic p (Backwards f) s t a b -> Optic p f s t a b
backwards lens get = a
  where
    a = rmap forwards $ lens $ rmap Backwards get

droppingWhile :: (Contravariant f, Applicative f) => LensLike (Const (DroppingWhileR (Ap f a))) s t a a -> (a -> Bool) -> LensLike f s t a a
droppingWhile lens predicate = droppingWhileBase lens (const predicate)

taking :: (Contravariant f, Applicative f) => LensLike (Const (TakeWhileApplicative f a)) s t a a -> Int -> LensLike f s t a a
taking lens n = takingWhileBase lens (\i _ -> i < n)

takingWhile :: (Contravariant f, Applicative f) => LensLike (Const (TakeWhileApplicative f a)) s t a a -> (a -> Bool) -> LensLike f s t a a
takingWhile lens predicate = takingWhileBase lens (const predicate)

takingWhileBase :: forall f s t a. (Contravariant f, Applicative f) => LensLike (Const (TakeWhileApplicative f a)) s t a a -> (Int -> a -> Bool) -> LensLike f s t a a
takingWhileBase lens predicate useA =
  lens (Const . createTakeWhile useA)
    >>> getConst
    >>> flip run noEffect
    >>> phantom
  where
    run :: TakeWhileApplicative f a -> f a -> f a
    run r b = runTakeWhileApplicative r 0 (const b)
    createTakeWhile :: (a -> f a) -> a -> TakeWhileApplicative f a
    createTakeWhile get a = TakeWhileApplicative (\i k -> if predicate i a then get a *> k (i + 1) else pure a)

noEffect :: (Contravariant f, Applicative f) => f a
noEffect = phantom $ pure ()

droppingWhileBase :: forall f s t a. (Contravariant f, Applicative f) => LensLike (Const (DroppingWhileR (Ap f a))) s t a a -> (Int -> a -> Bool) -> LensLike f s t a a
droppingWhileBase lens predicate useA =
  lens (Const . createDrop useA)
    >>> getConst
    >>> run
    >>> phantom
  where
    run :: Applicative f => DroppingWhileR (Ap f a) -> f a
    run r = getAp $ fst $ runDroppingWhileR r (0, True)
    createDrop :: (a -> f a) -> a -> DroppingWhileR (Ap f a)
    createDrop get a = DroppingWhileR $ \(i, _) -> (Ap $ get a, predicate i a)

droppingWhileBase2 :: forall f s t a. (Contravariant f, Applicative f) => LensLike (Const (DroppingWhileApplicative f a)) s t a a -> (Int -> a -> Bool) -> LensLike f s t a a
droppingWhileBase2 lens predicate useA =
  lens (Const . createDrop useA)
    >>> getConst
    >>> run
    >>> phantom
  where
    run :: Applicative f => DroppingWhileApplicative f a -> f a
    run r = runDroppingWhileApplicative r 0 noEffect
    createDrop :: (a -> f a) -> a -> DroppingWhileApplicative f a
    createDrop get a = DroppingWhileApplicative $ \i v -> if predicate i a then get a else pure a

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
preview = runGet (Just >>> XFirst) getXFirst

foldOf :: Getting a s a -> s -> a
foldOf useA = getConst . useA Const

foldMapOf :: Getting a s a -> (a -> r) -> s -> r
foldMapOf useA f = f . foldOf useA

foldrOf :: Getting (Endo r) s a -> (a -> r -> r) -> r -> s -> r
foldrOf getFold f r0 =
  runGet
    (f >>> Endo)
    (appEndo >>> ($ r0))
    getFold

foldlOf :: Getting (Dual (Endo r)) s a -> (r -> a -> r) -> r -> s -> r
foldlOf getFold f r0 =
  runGet
    (flip f >>> Endo >>> Dual)
    (getDual >>> appEndo >>> ($ r0))
    getFold

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
    (appEndo >>> ($ []))
    lens

ifoldMapOf :: IndexedGetting i r s a -> (i -> a -> r) -> s -> r
ifoldMapOf lens f = runIndexedGet f id lens

ianyOf :: IndexedGetting i Any s a -> (i -> a -> Bool) -> s -> Bool
ianyOf lens predicate = runIndexedGet (\i a -> Any $ predicate i a) getAny lens

itoListOf :: IndexedGetting i (Endo [(i, a)]) s a -> s -> [(i, a)]
itoListOf = runIndexedGet 
  (\i a -> Endo $ \l -> (i,a):l)
  (appEndo >>> ($ []))

-- ==================== OLD ====================

takingWhileSimple :: forall f s t a. (Contravariant f, Applicative f) => LensLike (Const (TakeWhile1 (Ap f a))) s t a a -> (Int -> a -> Bool) -> LensLike f s t a a
takingWhileSimple lens predicate useA =
  lens (Const . createTakeWhile useA)
    >>> getConst
    >>> run
    >>> phantom
  where
    run :: Applicative f => TakeWhile1 (Ap f a) -> f a
    run r = maybe noEffect getAp $ runTakeWhile1 r 0
    createTakeWhile :: (a -> f a) -> a -> TakeWhile1 (Ap f a)
    createTakeWhile get a = TakeWhile1 $ \i ->
      if predicate i a
        then Just (Ap $ get a)
        else Nothing
