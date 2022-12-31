{-# LANGUAGE QuasiQuotes #-}
module RPC.Base (
  module X,
) where
import RPC.Server as X
import RPC.Client as X
import RPC.Messaging as X
import RPC.Parse as X
import RPC.Samples as X


-- type R = RSIO () Int
[remote| myFn :: Int -> R   |]
