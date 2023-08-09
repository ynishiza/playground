{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module SimpleCStreamProperties
  ( properties,
  )
where

import Data.Either
import Data.Function
import Data.Functor
import Data.Functor.Sum
import Hedgehog
import Hedgehog.Gen qualified as H
import Hedgehog.Range qualified as H
import SimpleStream.CPrelude qualified as C
import SimpleStream.Of
import SimpleStream.Prelude qualified as S

numTests :: Num i => i
numTests = 200

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

cstreamFrom :: Monad m => [a] -> C.CStreamOf a m ()
cstreamFrom l =
  C.each l
    & C.mapcM pure

property_ :: PropertyT IO () -> Property
property_ = withTests numTests . property

properties :: Group
properties =
  Group
    "Basic stream vs CPS stream"
    [ ( "[each]",
        property_ $ do
          list <- forAll genNumberList_
          cover 1 "short stream (< 10)" $ length list < 10
          cover 10 "long stream (> 100)" $ length list > 100
          cstreamFrom list `streamEq` S.each list
      ),
      ( "[splitAt]",
        property_ $ do
          list <- forAll genNumberList_
          n <- forAll $ genIntRange 0 (length list)
          (csplit :> crest) <-
            cstreamFrom list
              & C.splitAtc n
              & C.toList
          (ssplit :> srest) <-
            S.each list
              & S.splitsAt n
              & S.toList

          cover 5 "small split position (< 10)" $ n < 10
          cover 5 "large split position (> 100)" $ n > 100
          csplit === ssplit
          crest `streamEq` srest
      ),
      ( "[groupc]",
        property_ $ do
          let splitList :: Of Int r -> Sum (Of Int) (Of Int) r
              splitList (x :> xs) = if even x then InL (x :> xs) else InR (x :> xs)
              collapse :: Either [a] [b] -> [Either a b]
              collapse = either (Left <$>) (Right <$>)
          list <- forAll genNumberList_
          l1 <-
            cstreamFrom list
              & C.mapc splitList
              & C.groupc
              & C.mapcM
                ( \case
                    (InL s) -> mapOf Left <$> C.toList s
                    (InR s) -> mapOf Right <$> C.toList s
                )
              & C.toList_
              <&> concatMap collapse
          l2 <-
            S.each list
              & S.maps splitList
              & S.groups
              & S.mapsM
                ( \case
                    (InL s) -> mapOf Left <$> S.toList s
                    (InR s) -> mapOf Right <$> S.toList s
                )
              & S.toList_
              <&> concatMap collapse

          let evens = lefts l1
              odds = rights l1
          cover 30 "More in right group" $ length evens > length odds
          cover 30 "More in left group" $ length odds > length evens
          l1 === l2
      ),
      ( "[chunk]",
        property_ $ do
          list <- forAll $ genNumberList 1 1000
          n <- forAll $ genIntRange 1 (length list)
          cover 5 "small chunk size (< 10)" $ n < 10
          cover 5 "large chunk (size > 100)" $ n > 100
          ( cstreamFrom list
              & C.chunkc n
              & C.mapcM C.toList
            )
            `streamEq` ( S.each list
                           & S.chunks n
                           & S.mapsM S.toList
                       )
      ),
      ( "[mapc,mapcM]",
        property_ $ do
          list <- forAll genNumberList_
          ( cstreamFrom list
              & C.mapcM (pure . mapOf (* 2))
            )
            `streamEq` ( S.each list
                           & S.mapsM (pure . mapOf (* 2))
                       )
      )
    ]
