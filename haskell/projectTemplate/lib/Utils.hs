{-# LANGUAGE OverloadedStrings #-}

module Utils (traceShow, traceShowId) where

import Debug.Trace (trace)
import Fmt

traceShow :: Show a1 => a1 -> a2 -> a2
traceShow x = trace (show x)

traceShowId :: Show a1 => String -> a1 -> a1
traceShowId label x = trace (label |+ " " +|| x ||+ "") x
