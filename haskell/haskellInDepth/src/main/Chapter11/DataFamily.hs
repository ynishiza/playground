{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Chapter11.DataFamily
  ( run,
  )
where

import Data.Foldable
import Fmt
import Utils

run :: TestState
run =
  createChapterTest
    "11.3.3"
    "DataFamily"
    ( do
        let ts =
              [ TS (),
                TS True
              ]
            test (TS v) = do
              let l1 = v `xcons` xempty
                  l0 = asTypeOf xempty l1
              fmtLn $
                  nameF "l0" (showBuilder l0)
                  <> nameF "l1" (showBuilder l1)
                  <> nameF "xhead l0" (showBuilder $ xhead l0)
                  <> nameF "xhead l1" (showBuilder $ xhead l1)
                  <> nameF "xcons" (showBuilder $ v `xcons` l0)
                  <> nameF "xcons" (showBuilder $ v `xcons` (v `xcons` l0))
              assertIsEqual 0 (xlength l0)
              assertIsEqual 1 (xlength l1)
         in traverse_ test ts
        testDone
    )

data TS = forall a. (Show a, Eq a, XListable a, Show (XList a)) => TS a

data family XList a

newtype instance XList () = MkXListUnit Int deriving (Show, Eq, Ord)

data instance XList Bool = MkXListBool Int Int deriving (Show, Eq, Ord)

class XListable a where
  xempty :: XList a
  xcons :: a -> XList a -> XList a
  xhead :: XList a -> Maybe a
  xlength :: XList a -> Int

instance XListable () where
  xempty = MkXListUnit 0
  xcons _ (MkXListUnit n) = MkXListUnit $ n + 1
  xhead (MkXListUnit n) = case n of 0 -> Nothing; _ -> Just ()
  xlength (MkXListUnit n) = n

instance XListable Bool where
  xempty = MkXListBool 0 0
  xcons v (MkXListBool t f) = if v then MkXListBool (t + 1) f else MkXListBool t (f + 1)
  xhead (MkXListBool t _) = if t > 0 then Just True else Nothing
  xlength (MkXListBool t f) = t + f
