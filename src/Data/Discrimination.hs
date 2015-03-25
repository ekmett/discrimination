module Data.Discrimination
  ( 
  -- * Data.Discrimination.Type
    Disc(..)
  , desc
  -- * Data.Discrimination.Class
  , Grouping(..)
  , Grouping1(..)
  , groupingBag
  , groupingSet
  , Sorting(..)
  , Sorting1(..)
  , sortingBag
  , sortingSet
  -- * Other Modules
  , module Data.Discrimination.Combinators
  ) where

import Data.Discrimination.Class
import Data.Discrimination.Combinators
import Data.Discrimination.Type
