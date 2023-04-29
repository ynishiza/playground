{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Lens.Proofs2 () where

import Data.Function
import Data.Functor.Const
import Data.Functor.Contravariant
import Data.Functor.Identity
import Lens.Get (Getting)
import Lens.Lens
import Lens.Monoid
import Lens.Prism (APrism, Splittable (Splittable), useSplittable)
import Lens.Set (ASetter)

toGetter :: (s -> a) -> Getter s t a b
toGetter f k s =
  k (f s)
    & phantom

get :: Getting r s a -> (a -> r) -> s -> r
get lens f s =
  lens (Const . f) s
    & getConst

toSetter :: (s -> a) -> (b -> t) -> Setter s t a b
toSetter f g ka s =
  ka (f s)
    & untainted
    & g
    & tainted

set :: ASetter s t a b -> (a -> b) -> s -> t
set lens f s =
  lens (tainted . f) s
    & untainted

toTraversal :: Traversable t => Traversal (t a) (t b) a b
toTraversal = traverse

toTraversalI :: Traversable t => IndexedTraversal Int (t a) (t b) a b
toTraversalI k s =
  traverse (\a -> Indexing $ \i -> (i + 1, indexed k i a)) s
    & execIndexing 0

toFold :: Foldable t => IndexedFold Int (t a) a
toFold k s =
  foldr
    (\a (i, r) -> (i + 1, indexed k i a *> r))
    (0 :: Int, phantom $ pure ())
    s
    & snd

toPrism :: (s -> Either t a) -> (b -> t) -> Prism s t a b
toPrism split merge k =
  right' k
    & dimap
      split
      ( \case
          Left t -> pure t
          Right b -> merge <$> b
      )

usePrism :: APrism s t a b -> (a -> b) -> s -> t
usePrism lens f s =
  lens (Splittable Right Identity)
    & useSplittable
      ( \split merge -> case split s of
          (Left t) -> runIdentity t
          (Right a) -> runIdentity $ merge (f a)
      )
