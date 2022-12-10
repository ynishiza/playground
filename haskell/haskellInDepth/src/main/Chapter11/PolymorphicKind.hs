{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Chapter11.PolymorphicKind
  ( run,
  )
where

import Data.Kind
import Data.Proxy
import Fmt
import Utils

type MyMono :: (a -> Type) -> a -> Type
newtype MyMono f a = MkMyMono (f a) deriving (Show)

type MyPoly :: forall a. (a -> Type) -> a -> Type
newtype MyPoly f a = MkMyPoly (f a) deriving (Show)

type MyLabel :: forall a. a -> Constraint
class MyLabel u where
  getLabel :: Proxy u -> String

instance MyLabel Int where
  getLabel _ = "mylabel: Int"

instance MyLabel Maybe where
  getLabel _ = "mylabel: Maybe"

run :: TestState
run =
  createChapterTest
    "11.2"
    "PolymorphicKind"
    ( do
        let xPoly :: MyPoly Maybe Int
            xPoly = MkMyPoly $ Just 1
            xMono :: MyMono Maybe Int
            xMono = MkMyMono $ Just 1
            yPoly :: MyPoly Maybe (Maybe Int)
            yPoly = MkMyPoly $ Just (Just 1)
            -- yMono :: MyMonoType Maybe (Maybe Int)      ERROR
            zPoly :: MyPoly Maybe (Maybe Int)
            zPoly = MkMyPoly $ Just Nothing
            polyNothing :: MyPoly Maybe (Maybe Int)
            polyNothing = MkMyPoly Nothing
         in do
              fmtLn $
                nameF "xPoly" (build $ show xPoly)
                  <> nameF "xMono" (build $ show xMono)
                  <> nameF "yPoly" (build $ show yPoly)
                  <> nameF "zPoly" (build $ show zPoly)
                  <> nameF "polyNothing" (build $ show polyNothing)

        fmtLn $
          nameF "int label" (build $ getLabel (Proxy @Int))
            <> nameF "maybe label" (build $ getLabel (Proxy @Maybe))
    )
