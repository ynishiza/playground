{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ConstraintKinds #-}

import Data.Kind
import Data.Singletons
import Data.Singletons.TH

genSingletons [''Bool, ''Maybe]

main :: IO ()
main = do
  print $ reifyProxy True
  print $ reifyProxy False
  print $ reifyProxy $ Just True
  print $ reifyProxy @(Maybe Bool) $ Nothing
  print $ MkSomeProxy (Proxy @Nothing)

  print $ mkSomeProxy STrue
  print $ mkSomeProxy SFalse
  print $ mkSomeProxy $ SJust SFalse
  print $ mkSomeProxy @(Maybe Bool) SNothing

type ProxyKind k = (SingKind k, Show (Demote k))
type SomeProxy :: Type
data SomeProxy where
  MkSomeProxy :: forall k (a :: k). (ProxyKind k, SingI a) => Proxy a -> SomeProxy

instance Show SomeProxy where
  show (MkSomeProxy p) = "SomeProxy: " ++ show (reflectProxy p)

mkSomeProxy :: forall k (a :: k). (ProxyKind k) => Sing a -> SomeProxy
mkSomeProxy s = withSingI s $ MkSomeProxy (Proxy @a)

reifyProxy :: forall k. ProxyKind k => Demote k -> SomeProxy
reifyProxy = use . toSing 
    where
      use (SomeSing (s :: Sing a)) = withSingI s $ MkSomeProxy (Proxy @a)

reflectProxy :: forall k (a :: k). (ProxyKind k, SingI a) => Proxy a -> Demote k
reflectProxy _ = fromSing (sing @a)
