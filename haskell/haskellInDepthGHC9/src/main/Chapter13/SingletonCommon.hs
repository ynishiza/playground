{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC  -Werror=incomplete-patterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE EmptyCase #-}

module Chapter13.SingletonCommon
  ( SBool (..),
    SMaybe (..),
    module X,
  )
where

import Data.Singletons as X
import Data.Singletons.TH
import Data.Singletons.Decide
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

instance SDecide Bool where
  STrue %~ STrue = Proved Refl
  SFalse %~ SFalse = Proved Refl
  STrue %~ SFalse = Disproved $ \case
  SFalse %~ STrue = Disproved $ \case

data SMaybe k where
  SNothing :: SMaybe 'Nothing
  SJust :: Sing x -> SMaybe ('Just x)

type instance Sing = SMaybe

instance SingI x => SingI ('Just x) where
  sing :: SMaybe ('Just x)
  sing = SJust (sing @x)

instance SingI 'Nothing where
  sing :: SMaybe 'Nothing
  sing = SNothing

instance SingKind k => SingKind (Maybe k) where
  type Demote (Maybe k) = Maybe (Demote k)

  fromSing :: Sing (l :: Maybe k) -> Demote (Maybe k)
  fromSing (SJust x) = Just $ fromSing x
  fromSing SNothing = Nothing

  toSing :: Demote (Maybe k) -> SomeSing (Maybe k)
  toSing Nothing = SomeSing SNothing
  toSing (Just x) = withSomeSing x (SomeSing . SJust)

instance SDecide a => SDecide (Maybe a) where
  SNothing %~ SNothing = Proved Refl
  SNothing %~ SJust _ = Disproved $ \case
  SJust _ %~ SNothing = Disproved $ \case
  SJust v  %~ SJust w = case v %~ w of 
                          Proved Refl  -> Proved Refl
                          Disproved rv -> Disproved $ \case Refl -> rv Refl


instance Show (SBool 'True) where show STrue = "STrue"

instance Show (SBool 'False) where show SFalse = "SFalse"

instance Show (SomeSing Bool) where
  show (SomeSing STrue) = "SomeSing " +|| STrue ||+ ""
  show (SomeSing SFalse) = "SomeSing " +|| SFalse ||+ ""

instance Show (SMaybe ('Just 'True)) where show (SJust STrue) = "SJust STrue"

instance Show (SMaybe ('Just ('Just 'True))) where show _ = "SJust $ SJust STrue"

instance Show (SMaybe ('Just 'False)) where show (SJust SFalse) = "SJust SFalse"

instance Show (SMaybe 'Nothing) where show _ = "SNothing"
