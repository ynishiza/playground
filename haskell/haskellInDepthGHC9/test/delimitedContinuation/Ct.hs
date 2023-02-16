{-# LANGUAGE FlexibleInstances #-}

module Ct
  ( Ct (..),
    ret,
    exit,
    (#>>=),
    (#>>),
    eval,
    reset,
    shift,
  )
where

-- Ct r i a
--
-- Generalized continuation
--
--   a = result of current computation
--   a -> i = child computation
--   r = output to parent computation
--
newtype Ct r i a = Ct {runCt :: (a -> i) -> r}

instance Functor (Ct r i) where
  fmap f (Ct c) = Ct $ \k -> c (k . f)

instance Applicative (Ct r r) where
  pure = ret
  (Ct cf) <*> (Ct c) = Ct $ \k -> cf (\f -> c (k . f))

instance Monad (Ct r r) where
  (Ct c) >>= f = Ct $ \k -> c (($ k) . runCt . f)

ret :: a -> Ct r r a
ret a = Ct ($ a)

exit :: r -> Ct r i a
exit = Ct . const

-- swap child from (a -> i) to (b -> j)
(#>>=) :: Ct r i a -> (a -> Ct i j b) -> Ct r j b
(Ct c) #>>= f = Ct $ \k -> c (\a -> runCt (f a) k)

(#>>) :: Ct r i a -> Ct i j b -> Ct r j b
c #>> d = c #>>= const d

infixl 1 #>>=, #>>

eval :: Ct r a a -> r
eval = ($ id) . runCt

reset :: Ct a b b -> Ct r r a
reset = ret . eval

shift :: ((a -> i) -> Ct r b b) -> Ct r i a
shift f = Ct $ \k -> eval (f k)
