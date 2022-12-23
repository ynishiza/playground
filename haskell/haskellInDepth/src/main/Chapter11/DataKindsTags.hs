{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Chapter11.DataKindsTags
  ( PersonRep (..),
    run,
  )
where

import Data.Foldable
import Data.Kind
import Data.Proxy
import GHC.TypeLits
import Utils

run :: TestState
run =
  createChapterTest
    "11.2"
    "Data kinds"
    ( do
        let test (PersonCont p) = do
              assertIsEqual (pidMeta p) (pid p)
              assertIsEqual (fullNameMeta p) (fullName p)
         in traverse_
              test
              [ PersonCont person1,
                PersonCont person2
              ]
        testDone
    )

-- data PersonCont = forall a b c d e. KnownNat a => PersonCont (PersonRep ('MkPersonMeta a b c d e))
data PersonCont = forall (a :: Nat) (b :: Status) (c :: Color) (d :: Symbol) (e :: Bool). (KnownNat a, KnownSymbol d) => PersonCont (PersonRep ('MkPersonMeta a b c d e))

type PersonMeta :: Type
data PersonMeta = MkPersonMeta !Nat !Status !Color !Symbol !Bool

data Color = Black | White

data Status = Approved | Denied

data PersonType = TypeA | TypeB | TypeC

type PersonRep :: PersonMeta -> Type
data PersonRep (a :: PersonMeta) = MkPersonRep
  { pid :: !Int,
    status :: !Status,
    color :: !Color,
    fullName :: !String,
    isAwesome :: !Bool
  }

person1 :: PersonRep ('MkPersonMeta 101 'Denied 'Black "Yui Nishizawa" 'False)
person1 =
  MkPersonRep
    { pid = 101,
      status = Denied,
      color = Black,
      fullName = "Yui Nishizawa",
      isAwesome = False
    }

person2 :: PersonRep ('MkPersonMeta 0 'Approved 'White "Alyosha Karamzov" 'True)
person2 =
  MkPersonRep
    { pid = 0,
      status = Approved,
      color = White,
      fullName = "Alyosha Karamzov",
      isAwesome = True
    }

pidMeta :: forall (a :: Nat) (b :: Status) (c :: Color) (d :: Symbol) (e :: Bool). KnownNat a => PersonRep ('MkPersonMeta (a :: Nat) b c d e) -> Int
pidMeta _ = fromInteger $ natVal (Proxy :: Proxy a)

fullNameMeta :: forall (a :: Nat) (b :: Status) (c :: Color) (d :: Symbol) (e :: Bool). KnownSymbol d => PersonRep ('MkPersonMeta a b c d e) -> String
fullNameMeta _ = symbolVal (Proxy :: Proxy d)
