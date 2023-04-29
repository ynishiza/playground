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
    elementOf,
    elementsOf,
    element,
    elements,

    both,
    beside,
    partsOf,
    holesOf,
  )
where
{- ORMOLU_ENABLE -}

import Data.Bitraversable
import Data.Function
import Data.Functor.Compose
import Lens.Lens
import Lens.Monoid.Monoid

type Traversing p s t a b = Over p (FreeAp a a) s t a b

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
        & f
        & (copyF fap <$>)
    copyF :: FreeAp a a x -> [a] -> x
    copyF fap l = fst $ replaceWith fap l

holesOf :: forall f s t a. Applicative f => Traversing (->) s t a a -> s -> [(a -> f a) -> f t]
holesOf lens s =
  lens FStore s
    & compute
  where
    compute :: FreeAp a a t -> [(a -> f a) -> f t]
    compute fap = holeAt fap l <$> [0 .. (length l - 1)]
      where
        l = foldStoreToList fap
    holeAt :: FreeAp a a t -> [a] -> Int -> (a -> f a) -> f t
    holeAt fap as i f =
      f a
        & ((initl ++) . (: restl) <$>)
        & (fst . replaceWith fap <$>)
      where
        a = as !! i
        initl = take i as
        restl = drop (i + 1) as

foldStoreToList :: FreeAp a o x -> [a]
foldStoreToList = storeFoldr (:) []

