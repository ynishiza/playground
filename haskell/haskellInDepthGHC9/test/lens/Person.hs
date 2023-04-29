module Person (Person (..), _name, _value) where

import Lens
import Data.Function ((&))

data Person a = Person
  { name :: String,
    value :: a
  }
  deriving (Show, Eq)

_name :: (ProfunctorArrow p, Functor f) => Optic' p f (Person a) String
_name k = 
  lmap name k
  & strong (\p v -> (\x -> (p {name = x})) <$> v)

_value :: (ProfunctorArrow p, Functor f) => Optic p f (Person a) (Person b) a b
_value k =
  lmap value k
  & strong (\p v -> (\x -> (p {value = x})) <$> v)
