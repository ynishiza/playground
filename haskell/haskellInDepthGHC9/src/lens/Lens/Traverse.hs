{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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
    elementOf,
    elementsOf,
    element,
    elements,

    both,
    beside,
    partsOf,
    holesOf,
    -- holesOf2,
    -- holesOf3,
  )
where
{- ORMOLU_ENABLE -}

import Data.Bitraversable
import Data.Coerce (coerce)
import Data.Function
import Data.Functor.Compose
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

holesOf :: forall p f s t a. (ProfunctorNormal p, Applicative f) => Traversing p s t a a -> s -> [p a (f a) -> f t]
holesOf lens s = holeOf <$> [0 .. length as - 1]
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
  lens (strong (\a x -> FStore x) p) s
    & g
  where
    g :: FreeAp (f a) a t -> f t
    g = undefined

newtype FT f a o x = FT (FreeAp a o (f x))

instance Functor f => Functor (FT f a o) where
  fmap g (FT x) = FT $ FFmap (g <$>) x

instance Applicative f => Applicative (FT f a o) where
  pure _ = undefined
  (FT f) <*> ~(FT x) = FT $ (<*>) <$> f <*> x

