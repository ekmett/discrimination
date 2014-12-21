module Data.Discrimination.Key
  ( Key(..)
  , key
  ) where

import Data.Discrimination.Order
import Data.Functor
import Data.Map as Map
import Data.Set as Set
import Data.Ord
import Data.Void

data Key
  = ConquerKey 
  | NaturalKey !Int
  | DivideKey !Key !Key
  | LeftKey !Key
  | RightKey !Key
  | BagKey !(Map Key Int)
  | SetKey !(Set Key)
  | DescKey !(Down Key)
  deriving (Eq,Ord,Show,Read)

key :: Order a -> a -> Key
key Conquer           _ = ConquerKey
key (Natural k _)     a = NaturalKey (k a)
key (Divide k l r)    a = case k a of
  (b, c) -> DivideKey (key l b) (key r c)
key (Lose k)          a = absurd (k a)
key (Choose k l r) a = case k a of
  Left b -> LeftKey (key l b)
  Right c -> RightKey (key r c)
key (Bag k r) a = BagKey $ Map.fromListWith (+) [ (key r b, 1) | b <- k a ]
key (Set k r) a = SetKey $ Set.fromList $ key r <$> k a
key (Desc k)  a = DescKey $ Down $ key k a
