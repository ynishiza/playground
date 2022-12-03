{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Chapter11.DataFamily
  ( run,
  )
where

import Data.Bits
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
              [ TestValue [(), ()],
                TestValue [True, False, True, False, False]
              ]
            testSingle :: Testable a => a -> IO (Int, XList a) -> IO (Int, XList a)
            testSingle v io = do
              (n, l) <- io
              let l' = v `xcons` l
              fmtLn $
                nameF "l'" (showBuilder l)
                  <> nameF "xlength" (showBuilder $ xlength l)
                  <> nameF "xhead l1" (showBuilder $ xhead l)
                  <> nameF "xcons" (showBuilder l')
              assertIsEqual (xhead l') (Just v)
              assertIsEqual (xlength l') (n + 1)
              return (n + 1, l')
            testMany (TestValue l) = do
              let l0 = xempty
              assertIsEqual (xhead l0) Nothing
              assertIsEqual (xlength l0) 0
              _ <- foldr testSingle (return (0, l0)) l
              pure ()
         in traverse_ testMany ts
        testDone
    )

type Testable a = (Show a, Eq a, XListable a, Show (XList a))

data TestValue = forall a. Testable a => TestValue [a]

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
  xcons v (MkXListBool b n) = MkXListBool (shift b 1 + if v then 1 else 0) (n + 1)
  xhead (MkXListBool b n) = if n > 0 then Just (b `mod` 2 == 1) else Nothing
  xlength (MkXListBool _ n) = n
