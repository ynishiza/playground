{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}

module Lens.Proofs () where
import Lens.Fold
import Lens.Get

isGetter_Getting :: Getter s s a a -> Getting r s a
isGetter_Getting = id

isFold_Getting :: Monoid r => Fold s a -> Getting r s a
isFold_Getting = id

isIndexedFold_Fold :: IndexedFold i s a -> Fold s a
isIndexedFold_Fold = id
