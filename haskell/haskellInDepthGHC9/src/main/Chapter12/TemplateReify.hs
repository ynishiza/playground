{-# LANGUAGE TemplateHaskell #-}

module Chapter12.TemplateReify
  ( mkPredicate,
    mkTypePredicates,
    Shape (..),
  )
where

import Language.Haskell.TH
import Utils

data Shape = Circle | Square | Triangle deriving (Show, Eq)

mkTypePredicates :: Name -> Q [Dec]
mkTypePredicates name = do
  (tName, cNames) <- getConstructorNames <$> reify name
  foldMap (mkPredicate tName) cNames

getConstructorNames :: Info -> (Name, [Name])
getConstructorNames (TyConI (DataD _ tName _ _ cons _)) =
  ( tName,
    getName <$> filter isNormal cons
  )
  where
    isNormal (NormalC _ _) = True
    isNormal _ = False
    getName (NormalC n _) = n
    getName _ = notImplemented "getConstructorNames"
getConstructorNames _ = notImplemented "getConstructorNames"

mkPredicate :: Name -> Name -> Q [Dec]
mkPredicate tName cName = do
  nameV <- newName "x"
  let predName = mkName $ "is" ++ nameBase cName
      sigBase = [t|$(conT tName) -> Bool|]
      exprBase = [|$(varE nameV) == $(conE cName)|]
  sig <- sigD predName sigBase
  expr <- funD predName [clause [varP nameV] (normalB exprBase) []]
  return [sig, expr]
