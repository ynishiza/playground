{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module Lens.TH (createFieldLenses, fieldLensBody, mkName) where

import Control.Arrow ((>>>))
import Control.Monad
import Data.Function
import Data.Set as S hiding (foldr)
import Language.Haskell.TH
import Lens.Class
import Lens.Lens
import TH.Utilities

createFieldLenses :: Name -> Q [Dec]
createFieldLenses =
  reify
    >=> ( \case
            TyConI d -> createFieldLensesForType d
            info -> fail $ "Not a type constructor:" <> show info
        )

createFieldLensesForType :: Dec -> Q [Dec]
createFieldLensesForType (DataD _ tyName tyVarBndrs _ dataCons _) =
  traverse buildLenses dataCons
    & (concat <$>)
  where
    tyVars = tyVarBndrName <$> tyVarBndrs
    tyInfo = (tyName, tyVars)
    buildLenses (RecC _ fieldVars) =
      traverse
        (\(fieldName, _, fieldType) -> createFieldLens (tyInfo, (fieldName, fieldType)))
        fieldVars
        & (concat <$>)
    buildLenses x = fail $ "Unsupported type" <> show x
createFieldLensesForType _ = fail "Not a data constructor"

createFieldLens :: (TypeConInfo, FieldInfo) -> Q [Dec]
createFieldLens info@((tyName, tyVarNames), (fieldName, fieldType)) = do
  (mappedTypeVars, mappedFieldType) <- mapFieldVars info
  pType <- newName "p"
  fType <- newName "f"
  let srcType = appsT (ConT tyName) (VarT <$> tyVarNames)
      dstType = appsT (ConT tyName) (VarT <$> mappedTypeVars)
      op :: Q Type
      op =
        [t|
          (ProfunctorArrow $(varT pType), Functor $(varT fType)) =>
          Optic $(varT pType) $(varT fType) $(pure srcType) $(pure dstType) $(pure fieldType) $(pure mappedFieldType)
          |]
  lensSignature <- sigD (createLensName fieldName) op
  lensExpression <- fieldLensBody fieldName
  return (lensSignature : lensExpression)

createLensName :: Name -> Name
createLensName name = mkName $ "_" <> nameBase name

fieldLensBody :: Name -> Q [Dec]
fieldLensBody fieldName = do
  [srcValue, x] <- replicateM 2 (newName "x")
  let updateExp :: Q Exp
      updateExp = recUpdE (varE srcValue) [pure (fieldName, VarE x)]
      lensName = mkName $ "_" <> nameBase fieldName
      updateLens =
        [|
          lmap $(varE fieldName)
            >>> strong (\ $(varP srcValue) v -> (\ $(varP x) -> $(updateExp)) <$> v)
          |]

  (: []) <$> funD lensName [clause [] (normalB updateLens) []]

type TypeConInfo = (Name, [Name])

type FieldInfo = (Name, Type)

mapFieldVars :: (TypeConInfo, FieldInfo) -> Q ([Name], Type)
mapFieldVars ((_, tyVarNames), (_, fieldType)) = do
  newVars <- replicateM (size toReplace) $ newName "a"
  let a = zip (S.toList toReplace) newVars
      x = foldr (flip replaceTypeVars) fieldType a
      y = foldr (flip replaceTypeVarNames) tyVarNames a
  return (y, x)
  where
    fieldVars = getTypeVars fieldType
    toReplace = intersection fieldVars (S.fromList tyVarNames)

getTypeVars :: Type -> Set Name
getTypeVars (AppT s t) = getTypeVars s `union` getTypeVars t
getTypeVars (AppKindT _ t) = getTypeVars t
getTypeVars (SigT t _) = getTypeVars t
getTypeVars (VarT n) = S.singleton n
getTypeVars (ConT n) = S.singleton n
getTypeVars _ = empty

replaceTypeVars :: Type -> (Name, Name) -> Type
replaceTypeVars _type i@(toReplace, replaced) = case _type of
  (AppT s t) -> AppT (replaceTypeVars s i) (replaceTypeVars t i)
  (AppKindT k t) -> AppKindT k $ replaceTypeVars t i
  (SigT t k) -> SigT (replaceTypeVars t i) k
  (VarT var) -> VarT $ replace var
  (ConT var) -> ConT $ replace var
  _ -> _type
  where
    replace n = if n == toReplace then replaced else n

replaceTypeVarNames :: [Name] -> (Name, Name) -> [Name]
replaceTypeVarNames names (n1, n2) = (\n -> if n == n1 then n2 else n) <$> names

-- appAllT :: NonEmpty Type -> Type
-- appAllT (a :| b:ts) = AppT a (appAllT $ b :| ts)
-- appAllT (a :| []) = a
