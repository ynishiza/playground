{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

{- ORMOLU_DISABLE -}
module Lens.Traverse
  ( IndexedTraversal,
    Traversal,
    IndexedTraversal',
    fromTraverse,
    toTraverse,
    traversal,
    traversed,
    traverseOf,
    sequenceAOf,
    failover,
    elementOf,
    elementsOf,
    element,
    elements,

    both,
    beside,
    partsOf,
    holesOf,
    holesOfA,
  )
where
{- ORMOLU_ENABLE -}

import Control.Applicative
import Data.Bitraversable
import Data.Coerce (coerce)
import Data.Function
import Data.Functor.Compose
import Data.Maybe (fromMaybe)
import Data.Monoid (Any (..))
import GHC.Stack (HasCallStack)
import Lens.Lens
import Lens.Monoid.Monoid

type Traversing p s t a b = forall f. Applicative f => Over p f s t a b

fromTraverse :: ((a -> f b) -> s -> f t) -> LensLike f s t a b
fromTraverse = id

toTraverse :: LensLike f s t a b -> (a -> f b) -> s -> f t
toTraverse = id

traversal :: ((a -> f b) -> s -> f t) -> LensLike f s t a b
traversal = fromTraverse

traversed :: Traversable t => IndexedTraversal Int (t a) (t b) a b
traversed pafb s =
  traverse (\a -> Indexing $ \i -> (i + 1, indexed pafb i a)) s
    & execIndexing 0

traverseOf :: LensLike f s t a b -> (a -> f b) -> s -> f t
traverseOf = toTraverse

sequenceAOf :: LensLike f s t (f b) b -> s -> f t
sequenceAOf lens = lens id

failover :: Alternative m => LensLike ((,) Any) s t a b -> (a -> b) -> s -> m t
failover lens f s = case lens (\a -> (Any True, f a)) s of
  (Any True, a) -> pure a
  _ -> empty

elementOf :: Applicative f => LensLike (Indexing f) s t a a -> Int -> IndexedLensLike Int f s t a a
elementOf lens n = elementsOf lens (== n)

elementsOf :: Applicative f => LensLike (Indexing f) s t a a -> (Int -> Bool) -> IndexedLensLike Int f s t a a
elementsOf lens predicate afb s =
  lens pick s
    & execIndexing 0
  where
    pick a = Indexing $ \i -> (i + 1, if predicate i then indexed afb i a else pure a)

element :: Traversable t => Int -> IndexedTraversal' Int (t a) a
element = elementOf traverse

elements :: Traversable t => (Int -> Bool) -> IndexedTraversal' Int (t a) a
elements = elementsOf traverse

both :: Bitraversable r => Traversal (r a a) (r b b) a b
both k = bitraverse k k

beside ::
  (ProfunctorRepresentation q, Applicative (Rep q), Applicative f, Bitraversable r) =>
  Optical p q f s t a b ->
  Optical p q f s' t' a b ->
  Optical p q f (r s s') (r t t') a b
beside lens1 lens2 k =
  fromRep
    ( \r ->
        bitraverse
          (Compose . toRep (lens1 k))
          (Compose . toRep (lens2 k))
          r
          & getCompose
    )

partsOf :: forall f s t a. Applicative f => Traversing (->) s t a a -> LensLike f s t [a] [a]
partsOf lens k s =
  lens FStore s
    & compute k
  where
    compute :: ([a] -> f [a]) -> FreeAp a a t -> f t
    compute f fap =
      foldStoreToList fap
        & (coerce <$>)
        & f
        & (copyF fap <$>)
    copyF :: FreeAp a a x -> [a] -> x
    copyF fap l = coerce $ fst $ replaceWith fap (coerce <$> l)

holesOf :: forall p f s t a. (ProfunctorNormal p, Functor f) => Traversing p s t a a -> s -> [p a (f a) -> f t]
holesOf lens s = holeAt <$> [0 .. length as - 1]
  where
    as = foldStoreToList $ lens (fromRep $ pure . FStore) s
    fap = lens (fromRep $ pure . FStore) s
    holeAt :: Int -> p a (f a) -> f t
    holeAt i p = (\a -> fst $ replaceWith fap (take i as <> [a])) <$> compute i p
    compute :: Int -> p a (f a) -> f a
    compute i0 p =
      lens (rmap (\b -> Indexing $ \i -> (i + 1, if i == i0 then MaybeConst (Just b) else MaybeConst Nothing)) p) s
        & flip runIndexing 0
        & snd
        & getConst2
        & fromMaybe (error "Should never happen")

newtype MaybeConst a x = MaybeConst (Maybe a)

getConst2 :: MaybeConst a x -> Maybe a
getConst2 (MaybeConst a) = a

instance Functor (MaybeConst a) where
  fmap _ (MaybeConst v) = MaybeConst v

instance Applicative (MaybeConst a) where
  pure _ = MaybeConst Nothing
  (MaybeConst (Just a)) <*> _ = MaybeConst (Just a)
  (MaybeConst Nothing) <*> (MaybeConst v) = MaybeConst v

-- note: holeOf with applicative
holesOfA :: forall p f s t a. (ProfunctorNormal p, Applicative f) => Traversing p s t a a -> s -> [p a (f a) -> f t]
holesOfA lens s = holeOf <$> [0 .. length as - 1]
  where
    as = foldStoreToList $ lens (fromRep $ pure . FStore) s
    holeOf :: Int -> p a (f a) -> f t
    holeOf i0 p =
      lens (strong (\a b -> Indexing $ \i -> (i + 1, if i == i0 then b else pure a)) p) s
        & flip runIndexing 0
        & snd

foldStoreToList :: FreeAp a o x -> [a]
foldStoreToList = storeFoldr (:) []

singular :: forall p f s t a. (HasCallStack, ProfunctorNormal p, Functor f) => Traversing p s t a a -> Over p f s t a a
singular lens p s =
  lens (strong (\_ x -> FStore x) p) s
    & g
  where
    g :: FreeAp (f a) a t -> f t
    g = undefined
