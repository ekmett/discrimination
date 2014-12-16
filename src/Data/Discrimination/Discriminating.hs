module Data.Discrimination.Discriminating where

import Data.Functor.Contravariant.Divisible

class Decidable t => Discriminating t where
  nat :: Int -> t Int
  bag :: t a -> t [a]
  set :: t a -> t [a]
