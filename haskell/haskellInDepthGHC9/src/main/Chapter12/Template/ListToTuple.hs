{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilyDependencies #-}

module Chapter12.Template.ListToTuple
  ( listToTupleDeclare,
    listToTupleDeclareMany,
  )
where

import Chapter12.Template.Utils
import Control.Monad
import Fmt
import Language.Haskell.TH

listToTupleDeclareMany :: Int -> Int -> Q [Dec]
listToTupleDeclareMany start end = foldMap listToTupleDeclare [start .. end]

listToTupleName :: Int -> Name
listToTupleName n = mkName $ "listToTuple_" +|| n ||+ ""

listToTupleDeclare :: Int -> Q [Dec]
listToTupleDeclare n = do
  names <- replicateM n newNameX
  let (_, _, mainClause) = listToTupleInfo names
      errorClause = clause [wildP] (normalB errorExp) []
  sig <- sigD fName $ listToTupleSignature n
  impl <- funD fName [pure mainClause, errorClause]
  pure [sig, impl]
  where
    fName = listToTupleName n
    errorExp = myTemplateErrorE $ "List must be exactly length " +| n |+ ""

listToTupleSignature :: Int -> Q Type
listToTupleSignature n = do
  a <- newName "a"
  simpleSignature
    [a]
    emptyCxt
    [t|$(pure $ listSignature a) -> $(pure $ tupleSignature (replicate n a))|]

listToTupleInfo :: [Name] -> (Pat, Exp, Clause)
listToTupleInfo names = (pat, expr, Clause [pat] (NormalB expr) [])
  where
    pat = ListP $ VarP <$> names
    expr = TupE $ Just . VarE <$> names
