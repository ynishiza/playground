{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use for_" #-}
module RadarSafeSpec (
  spec) where

import Test.Hspec
import Data.Foldable
import RadarSafe

spec :: SpecWith ()
spec = describe "Safe radar" $ do
  describe "rotation" $ do
    it "rotates clockwise" $ do
      rotateClockwiseIO rn >>= (`shouldBe` re)
      rotateClockwiseIO re >>= (`shouldBe` rs)
      rotateClockwiseIO rs >>= (`shouldBe` rw)
      rotateClockwiseIO rw >>= (`shouldBe` rn)

    it "rotates anticlockwise" $ do
      rotateAntiClockwiseIO rn >>= (`shouldBe` rw)
      rotateAntiClockwiseIO re >>= (`shouldBe` rn)
      rotateAntiClockwiseIO rs >>= (`shouldBe` re)
      rotateAntiClockwiseIO rw >>= (`shouldBe` rs)

    it "rotates north from any direction" $
      flip traverse_ allRadars $ \(_ :&: r) ->
        rotateToIO r SNorth >>= (`shouldBe` rn)

    it "rotates eact from any direction" $
      flip traverse_ allRadars $ \(_ :&: r) ->
        rotateToIO r SEast >>= (`shouldBe` re)

    it "rotates south from any direction" $
      flip traverse_ allRadars $ \(_ :&: r) ->
        rotateToIO r SSouth >>= (`shouldBe` rs)

    it "rotates west from any direction" $
      flip traverse_ allRadars $ \(_ :&: r) ->
        rotateToIO r SWest >>= (`shouldBe` rw)
