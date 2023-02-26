{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module SimpleCStreamProperties
  ( properties,
  )
where

import Data.Function
import Data.Functor.Sum
import Hedgehog
import Hedgehog.Gen qualified as H
import Hedgehog.Range qualified as H
import SimpleStream.CPrelude qualified as C
import SimpleStream.Of
import SimpleStream.Prelude qualified as S

genNumberList :: Int -> Int -> Gen [Int]
genNumberList lower upper = H.list (H.linear lower upper) genInt

genNumberList_ :: Gen [Int]
genNumberList_ = genNumberList 1 1000

genInt :: Gen Int
genInt = fromIntegral <$> H.int8 H.linearBounded

genIntRange :: Int -> Int -> Gen Int
genIntRange l h = H.int (H.linear l h)

streamEq :: (MonadTest m, Eq a, Show a, Eq r, Show r) => C.CStreamOf a m r -> S.StreamOf a m r -> m ()
streamEq cs s = do
  cr <- C.toList cs
  r <- S.toList s
  cr === r

testList :: Monad m => [a] -> C.CStreamOf a m ()
testList l =
  C.each l
    & C.mapcM pure

properties :: Group
properties =
  Group
    "CStream"
    [ ( "[each]",
        property $ do
          list <- forAll genNumberList_
          testList list `streamEq` S.each list
      ),
      ( "[splitAt]",
        property $ do
          list <- forAll genNumberList_
          n <- forAll $ genIntRange 0 (length list)
          (csplit :> crest) <-
            testList list
              & C.splitAtc n
              & C.toList
          (ssplit :> srest) <-
            S.each list
              & S.splitsAt n
              & S.toList

          csplit === ssplit
          crest `streamEq` srest
      ),
      ( "[groupc]",
        property $ do
          let 
              splitList :: Of Int r -> Sum (Of String) (Of String) r
              splitList (x :> xs) = if even x then InL ("left:" <> show x :> xs) else InR ("right:" <> show x :> xs)
          list <- forAll genNumberList_
          l1 <-
            testList list
              & C.mapc splitList
              & C.groupc
              & C.mapcM
                ( \case
                    (InL s) -> C.toList s
                    (InR s) -> C.toList s
                )
              & C.toList_
          l2 <-
            S.each list
              & S.maps splitList
              & S.groups
              & S.mapsM
                ( \case
                    (InL s) -> S.toList s
                    (InR s) -> S.toList s
                )
              & S.toList_
          l1 === l2
      ),
      ( "[chunk]",
        property $ do
          list <- forAll $ genNumberList 1 1000
          n <- forAll $ genIntRange 1 (length list)
          ( testList list
              & C.chunkc n
              & C.mapcM C.toList
            )
            `streamEq` ( S.each list
                           & S.chunks n
                           & S.mapsM S.toList
                       )
      ),
      ( "[mapc,mapcM]",
        property $ do
          list <- forAll genNumberList_
          ( testList list
              & C.mapcM (pure . mapOf (* 2))
            )
            `streamEq` ( S.each list
                           & S.mapsM (pure . mapOf (* 2))
                       )
      )
    ]
