{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module RPC.ParseUtils
  ( extractTopLevelSig,
    resolveSynonym,
    failOnTypeError,
  )
where

import Language.Haskell.TH
import RPC.Common

-- extract the top types of the signature.
-- In particular, the function arguments + result
-- e.g.
--    extractTopLevelSig [t| Int -> (Maybe a) -> Double) |]
--        ==> ([Int, Maybe a], Double)
--
extractTopLevelSig :: Type -> ([Type], Type)
extractTopLevelSig (ForallT _ _ t) = extractTopLevelSig t
extractTopLevelSig (AppT (AppT ArrowT t1) (AppT (AppT ArrowT t2) t3)) = (t1 : t2 : c, o)
  where
    (c, o) = extractTopLevelSig t3
extractTopLevelSig (AppT (AppT ArrowT t1) t2) = ([t1], t2)
extractTopLevelSig t = ([], t)

resolveSynonym :: Type -> Q Type
resolveSynonym (AppT t _) = resolveSynonym t
resolveSynonym t@(ConT name) = do
  info <- reify name
  case info of
    (TyConI (NewtypeD {})) -> pure t
    (TyConI (DataD {})) -> pure t
    (TyConI (TySynD _ _ ts)) -> resolveSynonym ts
    _ -> failOnTypeError "Not a type declaration" t
resolveSynonym t = failOnTypeError "Not a type declaration" t

failOnTypeError :: String -> Type -> Q a
failOnTypeError msg t = fail $ pretty @Builder $ msg |+ " type=" +|| t ||+ ""
