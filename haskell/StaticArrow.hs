module StaticArrow
  ( StaticArrow (..),
  )
where

import Control.Applicative
import Control.Arrow
import qualified Control.Category as Cat

-- StaticArrow: as defined in "Applicative programming with effects"
-- https://www.staff.city.ac.uk/~ross/papers/Applicative.html
newtype StaticArrow f arr a b = StaticArrow {getStaticArray :: f (arr a b)}

instance (Applicative f, Cat.Category an) => Cat.Category (StaticArrow f an) where
  id = StaticArrow $ pure Cat.id
  StaticArrow f . StaticArrow g = StaticArrow ((Cat..) <$> f <*> g)

instance (Applicative f, Arrow an) => Arrow (StaticArrow f an) where
  arr = StaticArrow . pure . arr
  first (StaticArrow f) = StaticArrow (first <$> f)

instance (Arrow an) => Cat.Category (WrappedArrow an) where
  id = WrapArrow Cat.id
  f . g = WrapArrow (unwrapArrow f Cat.. unwrapArrow g)

instance (Arrow an) => Arrow (WrappedArrow an) where
  arr = WrapArrow . arr
  first f = WrapArrow (first $ unwrapArrow f)
