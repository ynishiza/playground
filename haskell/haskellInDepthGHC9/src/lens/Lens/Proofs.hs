{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Lens.Proofs where

import Lens.Fold
import Lens.Get

isGetterGetting :: Getter s s a a -> Getting r s a
isGetterGetting = id

isGettingFold :: Monoid r => Fold s a -> Getting r s a
isGettingFold = id

isIndexedFold :: IndexedFold i s a -> Fold s a
isIndexedFold = id
