
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE ConstraintKinds #-}

module Chapter11.Unescaping0 (
  UnescapingChar(..),
  ushow0,
  uprint0,
  ) where

import Data.Kind
import GHC.Show (showLitChar)
import Unsafe.Coerce (unsafeCoerce)

newtype UnescapingChar = UnescapingChar {unescapingChar :: Char}

type ToUnescapingTF :: forall a. a -> a
type family ToUnescapingTF (a :: k) :: k where
  ToUnescapingTF Char = UnescapingChar
  ToUnescapingTF (t b :: k) = (ToUnescapingTF t) (ToUnescapingTF b)
  ToUnescapingTF a = a

class ToUnescaping a where
    toUnescaping :: a -> ToUnescapingTF a

instance Show a => ToUnescaping a where
    toUnescaping = unsafeCoerce

type UnescapingShow t = (ToUnescaping t, Show (ToUnescapingTF t))

ushow0 :: UnescapingShow t => t -> String
ushow0 = show . toUnescaping

uprint0 :: UnescapingShow t => t -> IO ()
uprint0 = putStrLn . ushow0
