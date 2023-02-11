module Ct
  ( Ct (..),
    Ctf,
    evalCtfWith,
    evalCtf,
    liftCt,
    liftct,
    pureCtf,
    resetCtf,
    resetCtfWith,
    mapOCtf,
  )
where

import Control.Monad.Free.Church

-- Ct r o a
--
-- Generalized continuation
--
--   a = result of computation
--   o = result of consumer
--   r = final output of function
--
newtype Ct r o a = Ct {runCt :: (a -> o) -> r}

instance Functor (Ct r o) where
  fmap f c = Ct $ \k -> runCt c (k . f)

type Ctf r o = F (Ct r o)

evalCtfWith :: (a -> r) -> (r -> o) -> Ctf r o a -> r
evalCtfWith r f (F m) = m r (($ f) . runCt)

evalCtf :: Ctf r r r -> r
evalCtf = evalCtfWith id id

liftCt :: Ct r o a -> Ctf r o a
liftCt = liftF

liftct :: ((a -> o) -> r) -> Ctf r o a
liftct = liftCt . Ct

pureCtf :: a -> Ctf r r a
pureCtf a = liftct ($ a)

resetCtf :: Ctf a a a -> Ctf r r a
resetCtf = pureCtf . evalCtf

resetCtfWith :: (a' -> a) -> (a -> o) -> Ctf a o a' -> Ctf r r a
resetCtfWith f g = pureCtf . evalCtfWith f g

mapO :: (o' -> o) -> Ct r o a -> Ct r o' a
mapO mp (Ct c) = Ct $ \k -> c (mp . k)

mapOCtf :: (o' -> o) -> Ctf r o a -> Ctf r o' a
mapOCtf mp = hoistF (mapO mp)
