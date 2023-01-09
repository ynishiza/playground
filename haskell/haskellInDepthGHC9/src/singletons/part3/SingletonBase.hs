{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module SingletonBase () where

import Data.Bool.Singletons
import Data.Singletons

type NotTrue = Not 'True
notTrueS = sing @NotTrue
type NotFalse = Not 'False
notFalseS = sing @NotFalse
type ManyAnd = NotTrue && NotTrue && NotFalse && 'True && 'False
manyAndS :: SBool 'False
manyAndS = sing @ManyAnd

snotTrue :: SBool 'False
snotTrue = sNot (sing @'True)
snotFalse :: SBool 'True
snotFalse = sNot (sing @'False)
sBoolComb :: SBool 'False
sBoolComb = snotTrue %&& snotFalse %&& (sing @'True)
sBoolComb2 :: SBool 'True
sBoolComb2 = snotTrue %||snotFalse %|| (sing @'True)
