{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module SingletonPlay
  ( test,
    SState (..),
    SBool (..),
    SMaybe (..),
  )
where

import SingletonCommon
import Data.Singletons
import Data.Singletons.TH
import Data.Typeable
import Fmt

data State where
  Good :: State
  Bad :: State
  deriving (Show, Eq)

genSingletons [''State]

instance Show (SState 'Good) where show SGood = "SGood"

instance Show (SState 'Bad) where show SBad = "SBAD"

instance Show (SomeSing State) where
  show (SomeSing SGood) = "SomeSing " +|| SGood ||+ ""
  show (SomeSing SBad) = "SomeSing " +|| SBad ||+ ""

instance Eq (SState 'Bad) where SBad == SBad = True

instance Eq (SState 'Good) where SGood == SGood = True

instance Eq (SomeSing State) where
  (SomeSing SGood) == (SomeSing SGood) = True
  (SomeSing SBad) == (SomeSing SBad) = True
  _ == _ = False

test = do
  let t =
        [ sing @'Good == SGood,
          sing @'Bad == SBad,
          fromSing SGood == Good,
          fromSing SBad == Bad,
          toSing Good == SomeSing SGood,
          toSing Bad == SomeSing SBad
        ]

  traverse print t
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
  printInfo "SJust STrue" (SJust STrue)
  printInfo "SJust STrue" (SJust $ SJust STrue)
  printInfo "sing @('Just 'True)" (sing @('Just 'True))
  print $ "sing @'Nothing" ++ show (sing @'Nothing)
  print $ "SNothing" ++ show SNothing

printInfo :: (Typeable a, Show a) => String -> a -> IO ()
printInfo name value = fmtLn $ name |+ "\t\t" +|| value ||+ " :: " +|| typeOf value ||+ ""
