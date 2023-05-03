-- run with
--
--   $ stack ghci -- src/scratch/genericsExample.hs
--
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

import Data.Typeable
import GHC.Generics

class FieldInfo m where
  getFieldInfo :: m a -> [(String, TypeRep)]

instance {-# OVERLAPPING #-} (Selector s, Typeable a) => FieldInfo (M1 S (s :: Meta) (K1 R a)) where
  getFieldInfo x@(M1 (K1 v)) = [(selName x, typeOf v)]

instance FieldInfo f => FieldInfo (M1 m (s :: Meta) f) where
  getFieldInfo (M1 x) = getFieldInfo x

instance (FieldInfo f, FieldInfo g) => FieldInfo (f :+: g) where
  getFieldInfo (L1 x) = getFieldInfo x
  getFieldInfo (R1 x) = getFieldInfo x

instance (FieldInfo f, FieldInfo g) => FieldInfo (f :*: g) where
  getFieldInfo (x :*: y) = getFieldInfo x ++ getFieldInfo y

data MyData = MyData {name :: String, value :: Int}
  deriving stock (Show, Eq, Generic)

myDataFields :: [(String, TypeRep)]
myDataFields = getFieldInfo $ from $ MyData "a" 1

test :: IO ()
test = do
  putStrLn $ "MyData fields:" <> show (getFieldInfo $ from $ MyData "a" 1)
