{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module SingletonPlay
  ( test,
    SState (..),
    SBool(..)
  )
where

import Data.Singletons
import Data.Singletons.TH
import Data.Typeable
import Fmt

data State where
  Good :: State
  Bad :: State
  deriving (Show, Eq)

genSingletons [''State]

data SBool s where
  STrue :: SBool 'True
  SFalse :: SBool 'False

type instance Sing = SBool 
instance SingI 'True where 
  sing :: SBool 'True
  sing = STrue
instance SingI 'False where 
  sing :: SBool 'False
  sing = SFalse
instance SingKind Bool where
  type Demote Bool = Bool
  fromSing :: SBool s -> Bool
  fromSing STrue = True
  fromSing SFalse = False

  toSing :: Bool -> SomeSing Bool
  toSing True = SomeSing STrue
  toSing False = SomeSing SFalse

instance Show (SState 'Good) where show SGood = "SGood"

instance Show (SState 'Bad) where show SBad = "SBAD"

instance Show (SBool 'True) where show STrue = "STrue"

instance Show (SBool 'False) where show SFalse = "SFalse"

instance Show (SomeSing Bool) where
  show (SomeSing STrue) = "SomeSing " +|| STrue ||+ ""
  show (SomeSing SFalse) = "SomeSing " +|| SFalse ||+ ""

instance Show (SomeSing State) where
  show (SomeSing SGood) = "SomeSing " +|| SGood ||+ ""
  show (SomeSing SBad) = "SomeSing " +|| SBad ||+ ""

instance Eq (SState 'Bad) where SBad == SBad = True

instance Eq (SState 'Good) where SGood == SGood = True

instance Eq (SomeSing State) where
  (SomeSing SGood) == (SomeSing SGood) = True
  (SomeSing SBad) == (SomeSing SBad) = True
  _ == _ = False

test =
  ( do
      let sg :: SState 'Good
          sg = SGood
          t =
            [ sing @'Good == SGood,
              sing @'Bad == SBad,
              fromSing SGood == Good,
              fromSing SBad == Bad,
              toSing Good == SomeSing SGood,
              toSing Bad == SomeSing SBad
            ]

      printInfo "SGood" SGood
      printInfo "SBAD" SBad
      printInfo "toSing Good" $ toSing Good
      printInfo "toSing Bad" $ toSing Bad
      printInfo "fromSing SGood" $ fromSing SGood
      printInfo "fromSing SBad" $ fromSing SBad
      printInfo "withSomeSing Good" $ withSomeSing Good SomeSing
      printInfo "withSomeSing Bad" $ withSomeSing Good SomeSing

      printInfo "STrue" STrue
      printInfo "SFalse" SFalse
  )

printInfo :: (Typeable a, Show a) => String -> a -> IO ()
-- printInfo name value = prettyLn $ build name <> ":" <> build (show value) <> " "  <> build (show (typeOf value))
printInfo name value = fmtLn $ name |+ ":" +|| value ||+ " (" +|| typeOf value ||+ ")"
