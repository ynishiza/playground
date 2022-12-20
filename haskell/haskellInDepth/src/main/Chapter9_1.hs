{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedStrings #-}

module Chapter9_1 (run) where

import Fmt
import GHC.Base (Char (..), Int (..))
import GHC.Prim
import Utils

run :: TestState
run =
  createChapterTest
    "9.1"
    "Primitives"
    ( do
        let i1 = I2# 1#
            i2 = I2# (2# +# 1#)
            c1 = C2# 'a'#
            c2 = C2# 'b'#
            i81 = I82# (narrowInt8# 1#)
            v :: Word8#
            v = narrowWord8# 1##
         in do
              fmt $
                nameF "i1" (build i1)
                  <> nameF "i2" (build i2)
                  <> nameF "c1" (build c1)
                  <> nameF "c2" (build c2)
                  <> nameF "i81" (build i81)
        testDone
    )

data Int2 = I2# Int#

data Char2 = C2# Char#

data Int82 = I82# Int8#

instance Show Int2 where show (I2# v) = show $ I# v

instance Buildable Int2 where build = showBuilder

instance Show Int82 where show (I82# v) = show $ fromIntegral @Int @Int $ I# (extendInt8# v)

instance Buildable Int82 where build = showBuilder

instance Show Char2 where show (C2# v) = show $ C# v

instance Buildable Char2 where build = showBuilder
