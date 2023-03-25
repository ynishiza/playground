{-# LANGUAGE TemplateHaskell #-}

module Chapter12.Template.Reify (genIsSimpleForConstructor, genIsSimple, genIs) where

import Language.Haskell.TH
import Utils

-- Generate is* functions for simple data
--
-- e.g.
--    data Alpha = A | B | C deriving Eq
--    $(genIsSimple ''Alpha)
--    isA :: Alpha -> True
--    isB :: Alpha -> True
--    isC :: Alpha -> True
--
genIsSimple :: Name -> Q [Dec]
genIsSimple name = do
  (tName, cNames) <- getConstructorNames <$> reify name
  foldMap (genIsSimpleForConstructor tName) cNames

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

genIsSimpleForConstructor :: Name -> Name -> Q [Dec]
genIsSimpleForConstructor tName cName = do
  nameV <- newName "x"
  let predName = mkName $ "is" ++ nameBase cName
      sigBase = [t|$(conT tName) -> Bool|]
      exprBase = [|$(varE nameV) == $(conE cName)|]
  sig <- sigD predName sigBase
  expr <- funD predName [clause [varP nameV] (normalB exprBase) []]
  return [sig, expr]

applyTypes :: Type -> [Type] -> Type
applyTypes = foldl AppT

-- Generate is* function for any data
--
-- e.g.
--    $(genIs ''Maybe)
--    isJust :: Maybe a -> Bool
--    isNothing :: Maybe a -> Bool
genIs :: Name -> Q [Dec]
genIs typeName = do
  (mainType, cs) <- reify typeName >>= getDataConstructorInfo
  concat <$> traverse (genIsForConstructor mainType) cs

getDataConstructorInfo :: Info -> Q (Type, [(Name, Int)])
getDataConstructorInfo (TyConI (DataD _ typeName typeVars _ dataCons _)) = do
  dataConInfo <- traverse getDataConInfo dataCons
  return (mainType, dataConInfo)
  where
    mainType = applyTypes (ConT typeName) $ VarT . mkName . ("a" <>) . show <$> [1 .. length typeVars]
    getDataConInfo (NormalC conc vars) = pure (conc, length vars)
    getDataConInfo (RecC conc vars) = pure (conc, length vars)
    getDataConInfo _ = fail ""
getDataConstructorInfo info = fail $ "Not a data type:" <> show info

genIsForConstructor :: Type -> (Name, Int) -> Q [Dec]
genIsForConstructor mainType (conName, conVars) = do
  let predicateName = mkName $ "is" <> nameBase conName
      conPattern = ConP conName [] (replicate conVars WildP)
  -- e.g.
  --    isJust :: Maybe a -> Bool
  --    isJust (Just _) = True
  --    isJust _ = False
  signature <-
    sigD
      predicateName
      [t|$(pure mainType) -> Bool|]
  expression <-
    funD
      predicateName
      [ clause [pure conPattern] (normalB [|True|]) [],
        clause [wildP] (normalB [|False|]) []
      ]
  return [signature, expression]
