module Data.Discrimination
  ( 
  -- * Type
    Disc(..)
  , desc
  -- * Discriminators
  -- ** Unordered
  , Grouping(..)
  , Grouping1(..)
  , groupingBag
  , groupingSet
  , groupingEq
  -- ** Ordered
  , Sorting(..)
  , Sorting1(..)
  , sortingBag
  , sortingSet
  , sortingCompare
  -- * Combinators
  -- ** Unordered
  , nub, nubWith, group, groupWith
  -- ** Ordered
  , sort, sortWith
  -- * Containers
  , toMap, toMapWith, toMapWithKey
  , toIntMap, toIntMapWith, toIntMapWithKey
  , toSet
  , toIntSet
  -- * Joins
  , joining, inner, outer, leftOuter, rightOuter
  ) where

import Data.Discrimination.Class
import Data.Discrimination.Combinators
import Data.Discrimination.Type
