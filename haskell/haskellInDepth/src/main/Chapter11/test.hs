
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ConstraintKinds #-}

import GHC.Show (showLitChar)
import Unsafe.Coerce (unsafeCoerce)

newtype UnescapingChar = UnescapingChar {unescapingChar :: Char}

type family ToUnescapingTF (a :: k) :: k where
  ToUnescapingTF Char = UnescapingChar
  ToUnescapingTF (t b :: k) = (ToUnescapingTF t) (ToUnescapingTF b)
  ToUnescapingTF a = a

class ToUnescaping a where
    toUnescaping :: a -> ToUnescapingTF a

instance Show a => ToUnescaping a where
    toUnescaping = unsafeCoerce

type UnescapingShow t = (ToUnescaping t, Show (ToUnescapingTF t))

ushow :: UnescapingShow t => t -> String
ushow = show . toUnescaping

uprint :: UnescapingShow t => t -> IO ()
uprint = putStrLn . ushow
