{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# OPTIONS_GHC -ddump-deriv #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Chapter12.Generics
  ( run,
    metaNorth,
    metaWest,
    metaSomeThingABC,
    SomeThing(..),
    SomeThingType,
    MyDirectionType,
    BoolType,
    MaybeType,
    EitherType
  )
where

import Data.Proxy
import GHC.Generics
import GHC.TypeLits
import Data.Kind
import Utils
import Fmt

run :: TestState
run =
  createChapterTest
    "12.2"
    "Generics"
    ( do
        assertIsEqual North (to metaNorth)
        assertIsEqual metaNorth (M1 (L1 (L1 consNorth)))
        assertIsEqual metaWest (M1 (R1 (R1 consWest)))

        assertIsEqual somethingABC (to metaSomeThingABC)
        assertIsEqual metaSomeThingABC (M1 (M1 (metaV1 "ABC" :*: (metaV2 "DEF" :*: metaV3 1))))

        assertIsEqual metaTrue (from True)
        assertIsEqual metaFalse (from False)
        assertIsEqual (to $ from True) True
        assertIsEqual metaFalse (from False)

        assertIsEqual metaJust1 (from (Just 1))
        assertIsEqual (metaNothing @Int) (from Nothing)

        assertIsEqual (metaLeftString @Int) (from (Left "ERROR"))
        assertIsEqual (metaRight1 @String) (from (Right 1))

        assertIsEqual metaPair (from (1, "A")) 

        fmtLn $ 
          nameF "module name" (build $ getModuleName (Proxy :: Proxy (SomeThingType Int ())))
          <> nameF "module name" (build $ moduleName $ from North)
          <> nameF "package name" (build $ packageName $ from North)
        testDone
    )

type MetaType = 'MetaData "MyType" "main" "Main" 'False

type MetaVal1 = 'MetaCons "GOOD" 'PrefixI 'False

type MetaVal2 = 'MetaCons "Bad" 'PrefixI 'False

data MyDirection = North | East | South | West deriving (Generic, Show, Eq)

getModuleName :: forall a b c d e f prox. KnownSymbol b => prox (M1 D ('MetaData a b c d) e f) -> String
getModuleName _ = symbolVal (Proxy :: Proxy b)

type MyDirectionType :: Type -> Type
type MyDirectionType x =
  M1
    D
    ('MetaData "MyDirection" "Chapter12.Generics" "main" 'False)
    ( ( M1 C ('MetaCons "North" 'PrefixI 'False) U1
          :+: M1 C ('MetaCons "East" 'PrefixI 'False) U1
      )
        :+: ( M1 C ('MetaCons "South" 'PrefixI 'False) U1
                :+: M1 C ('MetaCons "West" 'PrefixI 'False) U1
            )
    )
    x

data SomeThing a = MkSomeThing
  { v1 :: {-# UNPACK #-} !String,
    v2 :: String,
    v3 :: a
  }
  deriving (Generic, Show, Eq)

type SomeThingType :: Type -> Type -> Type
type SomeThingType a x =
  M1 D ('MetaData "SomeThing" "Chapter12.Generics" "main" 'False)
    (M1 C ('MetaCons "MkSomeThing" 'PrefixI 'True)
        (M1 S ('MetaSel ('Just "v1") 'SourceUnpack 'SourceStrict 'DecidedStrict) (K1 R String)
        :*: M1 S ('MetaSel ('Just "v2") 'NoSourceUnpackedness 'NoSourceStrictness 'DecidedLazy) (K1 R String)
        :*: M1 S ('MetaSel ('Just "v3") 'NoSourceUnpackedness 'NoSourceStrictness 'DecidedLazy) (K1 R a)
        )
    )
    x

y :: Rep (SomeThing Int) x
-- y :: SomeThingType Int x
y = from (MkSomeThing "" "" 1)

metaNorth :: Rep MyDirection x
-- metaNorth :: MyDirectionType x         -- Same, but fails for internal library since module is not "main"
metaNorth = from North

metaWest :: Rep MyDirection x
-- metaWest :: MyDirectionType x          -- Same, but fails for internal library since module is not "main"
metaWest = from West

consWest :: M1 C ('MetaCons "West" 'PrefixI 'False) U1 x
consWest = M1 U1

consNorth :: M1 C ('MetaCons "North" 'PrefixI 'False) U1 x
consNorth = M1 U1

metaV1 :: String -> M1 S ('MetaSel ('Just "v1") 'SourceUnpack 'SourceStrict 'DecidedUnpack) (K1 R String) x
metaV1 = M1 . K1

metaV2 :: String -> M1 S ('MetaSel ('Just "v2") 'NoSourceUnpackedness 'NoSourceStrictness 'DecidedLazy) (K1 R String) x
metaV2 = M1 . K1

metaV3 :: a -> M1 S ('MetaSel ('Just "v3") 'NoSourceUnpackedness 'NoSourceStrictness 'DecidedLazy) (Rec0 a) x
metaV3 = M1 . K1

somethingABC :: SomeThing Int
somethingABC = MkSomeThing "ABC" "DEF" 1

-- metaSomeThingABC :: SomeThingType x      -- NO: fails for internal library since module is not "main"
-- metaSomeThingABC :: SomeThingType Int x      -- NO: fails for internal library since module is not "main"
metaSomeThingABC = from somethingABC

type BoolType :: forall a. a -> Type
type BoolType x = 
  M1 D ('MetaData "Bool" "GHC.Types" "ghc-prim" 'False) (
    M1 C ('MetaCons "False" 'PrefixI 'False) U1 
    :+: 
    M1 C ('MetaCons "True" 'PrefixI 'False) U1
  ) x

metaTrue :: BoolType x
metaTrue = M1 (R1 (M1 U1))

-- metaTrue = from True

metaFalse :: BoolType x
-- metaFalse = from False
metaFalse = M1 (L1 (M1 U1))

type MaybeType :: forall a. Type -> a -> Type
type MaybeType a x =
  M1 D ('MetaData "Maybe" "GHC.Maybe" "base" 'False) ( 
    M1 C ('MetaCons "Nothing" 'PrefixI 'False) U1
    :+: 
    M1 C ('MetaCons "Just" 'PrefixI 'False)
          (M1 S ('MetaSel 'Nothing 'NoSourceUnpackedness 'NoSourceStrictness 'DecidedLazy) (K1 R a))
  ) x

metaJust1 :: MaybeType Int x
metaJust1 = M1 (R1 (M1 (M1 (K1 1))))

-- metaJust1 = from (Just 1)

metaNothing :: MaybeType a x
-- metaNothing = from Nothing
metaNothing = M1 (L1 (M1 U1))

type EitherType :: forall a. Type -> Type -> a -> Type
type EitherType a b x =
  M1 D ('MetaData "Either" "Data.Either" "base" 'False) ( 
    M1 C ('MetaCons "Left" 'PrefixI 'False)
      (M1 S ('MetaSel 'Nothing 'NoSourceUnpackedness 'NoSourceStrictness 'DecidedLazy) (K1 R a))
    :+: 
    M1 C ('MetaCons "Right" 'PrefixI 'False)
      (M1 S ('MetaSel 'Nothing 'NoSourceUnpackedness 'NoSourceStrictness 'DecidedLazy) (K1 R b))
    ) x

metaLeftString :: EitherType String a x
metaLeftString = M1 (L1 (M1 ( M1 (K1 "ERROR"))))
-- metaLeftString = from $ Left "ERROR"

metaRight1 :: EitherType a Int x
-- metaRight1 = from $ Right 1
metaRight1 = M1 (R1 (M1 ( M1 (K1 1))))

type PairType a b x = 
  M1 D ('MetaData "(,)" "GHC.Tuple" "ghc-prim" 'False) ( 
    M1 C ('MetaCons "(,)" 'PrefixI 'False) (
      M1 S ('MetaSel 'Nothing 'NoSourceUnpackedness 'NoSourceStrictness 'DecidedLazy) (K1 R a)
    :*: 
      M1 S ('MetaSel 'Nothing 'NoSourceUnpackedness 'NoSourceStrictness 'DecidedLazy) (K1 R b)
                                           )
    ) x

-- metaPair :: PairType Int String x
metaPair :: Rep (Int, String) x
metaPair = M1 (M1 (M1 (K1 1) :*: M1 (K1 "A")))
-- metaPair = from (1, "A")
