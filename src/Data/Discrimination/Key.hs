module Data.Discrimination.Key
  ( Key(..)
  , key
  ) where

import Data.Discrimination.Order
import Data.Discrimination.Type
import Data.Functor.Contravariant
import Data.Functor.Contravariant.Divisible
import Data.Ord

data Key
  = ConquerKey 
  | NaturalKey !Int
  | DivideKey !Key !Key
  | LeftKey !Key
  | RightKey !Key
  | BagKey !(Map Key Int)
  | SetKey !(Set Key)
  | InverseKey !(Down Key)
  deriving (Eq,Ord,Show,Read)

key :: Order a -> a -> Key
key Conquer           _ = ConquerKey
key (Natural k n)     a = NaturalKey (k a)
key (Divide k l r)    a = case k a of
  (b, c) -> DivideKey (key l b) (key r c)
key (Lose k)          a = absurd (k a)
key (ChooseKey k l r) a = 
  Left b -> LeftKey (key l b)
  Right c -> RightKey (key r c)
key (BagKey k r) a = Map.fromListWith (+) [ (r b, 1) | b <- k a ]
key (SetKey k r) a = Set.fromList $ r <$> k a)
key (Inverse k)  a = InverseKey $ Down $ key k a
