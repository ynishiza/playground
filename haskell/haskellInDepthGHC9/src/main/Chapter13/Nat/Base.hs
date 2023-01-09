{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}

module Chapter13.Nat.Base where

import Chapter13.Nat.TH
import Data.Type.Nat

$(genNumAliases 0 100)
