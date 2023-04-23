{-# LANGUAGE LambdaCase #-}

module Lens.TH.Base
  ( genFieldLenses,
    genConstructorPrisms,
  )
where

import Control.Monad
import Language.Haskell.TH
import Lens.TH.DataConstructor
import Lens.TH.Record

genFieldLenses :: Name -> Q [Dec]
genFieldLenses =
  reify
    >=> ( \case
            TyConI d -> genAllFieldLensesForType d
            info -> fail $ "Not a type constructor:" <> show info
        )

genConstructorPrisms :: Name -> Q [Dec]
genConstructorPrisms =
  reify
    >=> ( \case
            TyConI d -> genAllConstructorPrisms d
            info -> fail $ "Not a type constructor:" <> show info
        )
