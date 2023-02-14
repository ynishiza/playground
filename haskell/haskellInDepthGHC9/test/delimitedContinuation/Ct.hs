{-# LANGUAGE FlexibleInstances #-}

module Ct
  ( Ct (..),
    ret,
    exit,
    (#>>=),
    eval,
    reset,
    shift,
  )
where

-- Ct r o a
--
-- Generalized continuation
--
--   a = output of computation
--   a -> o = consumer
--   r = final output of function
--
newtype Ct r o a = Ct {runCt :: (a -> o) -> r}

instance Functor (Ct r o) where
  fmap f (Ct c) = Ct $ \k -> c (k . f)

instance Applicative (Ct r r) where
  pure = ret
  (Ct cf) <*> (Ct c) = Ct $ \k -> cf (\f -> c (k . f))

instance Monad (Ct r r) where
  (Ct c) >>= f = Ct $ \k -> c (($ k) . runCt . f)

ret :: a -> Ct r r a
ret a = Ct ($ a)

exit :: a -> Ct a o b
exit = Ct . const

(#>>=) :: Ct r o a -> (a -> Ct o o' b) -> Ct r o' b
(Ct c) #>>= f = Ct $ \k -> c (\a -> runCt (f a) k)

eval :: Ct r a a -> r
eval = ($ id) . runCt

reset :: Ct r a a -> Ct r' r' r
reset c = Ct $ \k -> k (eval c)

shift :: ((a -> o) -> Ct r b b) -> Ct r o a
shift f = Ct $ \k -> eval (f k)
