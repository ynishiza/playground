{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use print" #-}

module Chapter11.Unescaping
  ( UnEscapedChar (..),
    Unescapable (..),
    testValue,
    ushow,
    uprint,
  )
where

import GHC.Show (showLitChar)
import Unsafe.Coerce
import Utils

testValue :: Char
testValue = 'あ'

-- instance Show UnEscapedChar where
--   show (MkUnEscapedChar c) = show c

instance Show UnEscapedChar where
  showsPrec _ (MkUnEscapedChar '\'') = showString "'\\''"
  showsPrec _ (MkUnEscapedChar c) =
    showChar '\'' . showLitChar' c . showChar '\''

  showList cs =
    showChar '"' . showLitString' (map unescapedChar cs)
      . showChar '"'

showLitChar' :: Char -> ShowS
showLitChar' c s | c > '\DEL' = showChar c s
showLitChar' c s = showLitChar c s

showLitString' :: String -> ShowS
showLitString' [] s = s
showLitString' ('"' : cs) s = showString "\\\"" (showLitString' cs s)
showLitString' (c : cs) s = showLitChar' c (showLitString' cs s)

newtype UnEscapedChar = MkUnEscapedChar {unescapedChar :: Char}

type UnEscapedCharOf :: forall a. a -> a
type family UnEscapedCharOf a where
  UnEscapedCharOf Char = UnEscapedChar
  UnEscapedCharOf (t b) = (UnEscapedCharOf t) (UnEscapedCharOf b)
  UnEscapedCharOf a = a

class Unescapable a where
  toUnescape :: a -> UnEscapedCharOf a

instance Show a => Unescapable a where
  toUnescape = unsafeCoerce

ushow :: (Unescapable a, Show (UnEscapedCharOf a)) => a -> String
ushow = show . toUnescape

uprint :: (Unescapable a, Show (UnEscapedCharOf a)) => a -> IO ()
uprint = putStrLn . show . toUnescape
