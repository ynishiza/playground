{-# LANGUAGE TypeFamilies #-}

module ScratchSpace (
  ) where

import Data.Coerce
import Unsafe.Coerce

type family IdF a where
  IdF a = Wrap1 a

data Wrap1 a = Wrap1 a deriving (Show, Eq)

newtype WrapFamily a = WrapFamily (IdF a)
deriving instance Eq a => Eq (WrapFamily a)
deriving instance Show a => Show (WrapFamily a)

unsafeWrap :: WrapFamily Int  -> WrapFamily (Wrap1 Int)
-- unsafeWrap = coerce
unsafeWrap = unsafeCoerce
