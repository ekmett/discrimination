module Data.Discrimination
  ( 
  -- * Discrimination
    Discriminating(..)
  -- * Unordered
  , Group(..)
  , Grouping(..)
  , Grouping1(..)
  , nub
  , nubWith
  , group
  , groupWith
  , groupingBag
  , groupingSet
  , groupingEq
  -- * Ordered
  , Sort(..)
  , Sorting(..)
  , Sorting1(..)
  , desc
  , sort
  , sortWith
  , sortingBag
  , sortingSet
  , sortingCompare
  -- * Container Construction
  , toMap
  , toMapWith
  , toMapWithKey
  , toIntMap
  , toIntMapWith
  , toIntMapWithKey
  , toSet
  , toIntSet
  -- * Joins
  , joining
  , inner
  , outer
  , leftOuter
  , rightOuter
  ) where

import Data.Discrimination.Class
import Data.Discrimination.Grouping
import Data.Discrimination.Sorting
