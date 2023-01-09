{-# LANGUAGE GADTs #-}
module Decision () where

import Data.Void

type Refuted a = a -> Void

data Decision a where
  Proved :: a -> Decision a
  Disproved :: Refuted a -> Decision a

