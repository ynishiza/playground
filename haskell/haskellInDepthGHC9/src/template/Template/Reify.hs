{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module Template.Reify (getNameInfo) where

import Data.Functor
import Language.Haskell.TH

getNameInfo :: Name -> Q Exp
getNameInfo name = do
  (a, b) <- _getNameInfo name
  [|(a, b)|]

_getNameInfo :: Name -> Q (String, String)
_getNameInfo name =
  reify name <&> \case
    ClassI {} -> ("class", nameBase name)
    ClassOpI {} -> ("class method", nameBase name)
    TyConI {} -> ("type", nameBase name)
    FamilyI {} -> ("type family", nameBase name)
    PrimTyConI {} -> ("primitive type constructor", nameBase name)
    DataConI {} -> ("data", nameBase name)
    PatSynI {} -> ("pattern", nameBase name)
    VarI {} -> ("variable", nameBase name)
    TyVarI {} -> ("type variable", nameBase name)
