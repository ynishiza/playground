{-# LANGUAGE TemplateHaskell #-}

module RPC.Parse (
  remote,
  typeArity
                 ) where

import Control.Monad.Catch
import Language.Haskell.Meta.Parse
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import qualified Data.Text as T

remote =
  QuasiQuoter
    { quoteDec = quoteDecRemote
    }

quoteDecRemote :: String -> Q [Dec]
quoteDecRemote s = do
  infos <- proc s
  concat <$> traverse genStubCaller infos

data FnInfo = MkFnInfo
  { fnName :: Name,
    fnType :: Type
  } deriving (Eq, Show)

proc :: MonadThrow m => String -> m [FnInfo]
proc s = 
  case parseDecs s of
    (Right decs) -> pure $ decToFn <$> decs
    (Left e) -> error e
  -- where
  --   ps v = do
  --     case parseDecs
  --   ls = filter null $ trim <$> lines s
genStubCaller :: FnInfo -> Q [Dec]
genStubCaller = undefined

decToFn :: Dec -> FnInfo
decToFn (SigD fnName fnType) = MkFnInfo fnName fnType

curryN :: Int -> Q Exp
curryN 1 = [| id |]
curryN n = [| curry . $(curryN (n - 1)) |]

typeArity :: Type -> Int
typeArity (ForallT _ _ t) = typeArity t
typeArity (AppT (AppT ArrowT _) m) = 1 + typeArity m
typeArity _ = 0

trim :: String -> String
trim = T.unpack . T.strip . T.pack
