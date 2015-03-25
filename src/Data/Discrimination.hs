module Data.Discrimination
  ( 
  -- * Data.Discrimination.Type
    Disc(..)
  -- * Data.Discrimination.Join
  , inner
  , outer
  , leftOuter
  , rightOuter
  -- * Data.Discrimination.Class
  , Grouping(..)
  , Grouping1(..)
  , Sorting(..)
  , Sorting1(..)
  -- * Data.Discrimination.Table
  , Table(..)
  -- * Other Modules
  , module Data.Discrimination.Combinators
  ) where

import Data.Discrimination.Class
import Data.Discrimination.Combinators
import Data.Discrimination.Table
import Data.Discrimination.Join
import Data.Discrimination.Type
