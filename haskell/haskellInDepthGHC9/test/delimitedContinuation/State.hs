{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Collapse lambdas" #-}
{-# HLINT ignore "Avoid lambda" #-}
module State
  ( get,
    tick,
    put,
    modify,
    CtState,
  )
where

import Ct

-- Representing state as a continuation
-- i.e.
--      continuation k = a -> state -> state        a = result of current
--                                                  first state = current state
--                                                  second state = new state
type CtState state a = Ct (state -> state) (state -> state) a

get :: CtState s s
get = Ct $ \k -> (\state -> k state state)

tick :: CtState Int ()
tick = Ct $ \k -> (\state -> k () (state + 1))

put :: s -> CtState s ()
put = modify . const

modify :: (s -> s) -> CtState s ()
modify f = Ct $ \k -> (\state -> k () (f state))
