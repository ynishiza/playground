{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Chapter11.PolyKinds
  ( run,
  )
where

import Data.Kind
import Data.Proxy
import Fmt
import Utils

run :: TestState
run =
  createChapterTest
    "11.2"
    "PolyKinds"
    ( do
        let xPoly :: MyPoly Maybe Int
            xPoly = MkMyPoly $ Just 1
            xMono :: MyMono Maybe Int
            xMono = MkMyMono $ Just 1
            yPoly :: MyPoly Maybe (Maybe Int)
            yPoly = MkMyPoly $ Just (Just 1)
            -- cMonoCont :: MyMono MyIntCont []         ERROR
            -- cMonoCont = MkMyMono (MkMyIntCont [1])
            cPolyCont :: MyPoly MyIntCont []
            cPolyCont = MkMyPoly (MkMyIntCont [1])
         in do
              fmtLn $
                nameF "xPoly" (build $ show xPoly)
                  <> nameF "xMono" (build $ show xMono)
                  <> nameF "yPoly" (build $ show yPoly)
                  <> nameF "cPolyCont" (build $ show cPolyCont)

        fmtLn $
          nameF "int label" (build $ getLabel (Proxy @Int))
            <> nameF "maybe label" (build $ getLabel (Proxy @Maybe))
    )

type MyMono :: (Type -> Type) -> Type -> Type
newtype MyMono f a = MkMyMono (f a) deriving (Show)

-- type MyPoly :: forall a. (a -> Type) -> a -> Type
type MyPoly :: forall a. (a -> Type) -> a -> Type
newtype MyPoly f a = MkMyPoly (f a) deriving (Show)

newtype MyIntCont c = MkMyIntCont (c Int)

instance Show (MyIntCont []) where
  show (MkMyIntCont v) = show v

type MyLabel :: forall a. a -> Constraint
class MyLabel u where
  getLabel :: Proxy u -> String

instance MyLabel Int where
  getLabel _ = "mylabel: Int"

instance MyLabel Maybe where
  getLabel _ = "mylabel: Maybe"
