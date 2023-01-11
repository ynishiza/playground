{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Defunctionalization
  ( test,
    APPLY,
    ID,
    IDSym0,
    NOTSym0,
    FOLDR,
    FOLDRSym2,
    FOLDRSym0,
    FOLDRSym1,
    type (~>),
    type (@@),
  )
where

import Data.Foldable
import Data.Kind
import Data.Proxy
import Data.Typeable (Typeable)
import Utils

data TyFunc a b

type a ~> b = TyFunc a b -> Type

type APPLY :: (a ~> b) -> a -> b
type family APPLY f x

type f @@ a = APPLY f a

infixr 1 @@

-- case: ID
type family ID x where
  ID x = x

type IDSym0 :: forall a. a ~> a
data IDSym0 a'

type instance APPLY IDSym0 x = ID x

-- case: Not
type family NOT x where
  NOT 'True = 'False
  NOT 'False = 'True

type NOTSym0 :: Bool ~> Bool
data NOTSym0 a

type instance APPLY NOTSym0 x = NOT x

-- case: And
type AND :: Bool -> Bool -> Bool
type family AND x y where
  AND 'True 'True = 'True
  AND _ _ = 'False

type ANDSym0 :: Bool ~> (Bool ~> Bool)
data ANDSym0 a'

type ANDSym1 :: Bool -> Bool ~> Bool
data ANDSym1 a b'

type ANDSym2 a b = AND a b

type instance APPLY ANDSym0 x = ANDSym1 x

type instance APPLY (ANDSym1 x) y = AND x y

-- case: type constructor application
type TyCon :: forall a b. (a -> b) -> (a ~> b)
data TyCon t a'

type instance APPLY (TyCon t) x = t x

-- case: foldr
type FOLDR :: (a ~> (b ~> b)) -> b -> [a] -> b
type family FOLDR f x l where
  FOLDR f x '[] = x
  FOLDR f x (s ': ss) = (f @@ s) @@ FOLDR f x ss

type FOLDRSym0 :: (a ~> (b ~> b)) ~> (b ~> ([a] ~> b))
data FOLDRSym0 a'

type FOLDRSym1 :: (a ~> (b ~> b)) -> (b ~> ([a] ~> b))
data FOLDRSym1 f a'

type FOLDRSym2 :: (a ~> (b ~> b)) -> b -> ([a] ~> b)
data FOLDRSym2 f a b'

type instance APPLY FOLDRSym0 f = FOLDRSym1 f

type instance APPLY (FOLDRSym1 f) b = FOLDRSym2 f b

type instance APPLY (FOLDRSym2 f b) l = FOLDR f b l

data ListHolder = forall (l :: [Bool]). Typeable l => MkHolder (Proxy l)

boolList =
  [ MkHolder (Proxy @L1),
    MkHolder (Proxy @L2),
    MkHolder (Proxy @L3)
  ]

type L1 = '[]

type L2 = 'True ': '[]

type L3 = 'False ': 'True ': '[]

test :: IO ()
test = do
  printWithLabel "Not True" $ showType (Proxy @(NOTSym0 @@ 'True))
  printWithLabel "Not True" $ showType (Proxy @(NOTSym0 @@ 'False))

  printWithLabel "ID 1" $ showType (Proxy @(IDSym0 @@ 1))
  printWithLabel "ID 2" $ showType (Proxy @(IDSym0 @@ 2))
  printWithLabel "ID \"ABC\"" $ showType (Proxy @(IDSym0 @@ "ABC"))

  printWithLabel "True AND True" $ showType (Proxy @((ANDSym0 @@ 'True) @@ 'True))
  printWithLabel "True AND False" $ showType (Proxy @((ANDSym0 @@ 'True) @@ 'False))
  printWithLabel "False AND True" $ showType (Proxy @((ANDSym0 @@ 'False) @@ 'True))
  printWithLabel "False AND False" $ showType (Proxy @((ANDSym0 @@ 'False) @@ 'False))

  printWithLabel "Maybe Int" $ showType (Proxy @(TyCon Maybe @@ Int))
  printWithLabel "[Int]" $ showType (Proxy @(TyCon [] @@ Int))
  printWithLabel "'Just 'True" $ showType (Proxy @(TyCon 'Just @@ 'True))

  printWithLabel "FOLDR AND 'True []" $ showType (Proxy @(FOLDR ANDSym0 'True L1))
  printWithLabel "FOLDR AND 'True ('False ': '[])" $ showType (Proxy @(FOLDR ANDSym0 'True L2))
  printWithLabel "FOLDR AND 'True ('True ': '[])" $ showType (Proxy @(FOLDR ANDSym0 'True L3))

  printWithLabel "FOLDRSym0 AND 'True []" $ showType (Proxy @(((FOLDRSym0 @@ ANDSym0) @@ 'True) @@ L1))
  printWithLabel "FOLDRSym0 AND 'True ('False ': '[])" $ showType (Proxy @(((FOLDRSym0 @@ ANDSym0) @@ 'True) @@ L2))
  printWithLabel "FOLDRSym0 AND 'True ('True ': '[])" $ showType (Proxy @(((FOLDRSym0 @@ ANDSym0) @@ 'True) @@ L3))
  flip traverse_ boolList $
    ( \(MkHolder (_ :: Proxy (l :: [Bool]))) -> do
        let x = Proxy @(((FOLDRSym0 @@ ANDSym0) @@ 'True) @@ l)
        printWithLabel "FOLDRSym0 AND 'True []" $ showType (Proxy @(((FOLDRSym0 @@ ANDSym0) @@ 'True) @@ L1))
    )
