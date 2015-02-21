module Data.Discrimination
  ( 
  -- * Data.Discrimination.Type
    Disc(..)
  -- * Data.Discrimination.Join
  , Join(..)
  , inner
  -- * Portions of Data.Discrimination.Class
  , Disorderable
  , Disorder(..)
  , Disorder1(..)
  , Orderable
  , Order(..)
  , Order1(..)
  -- * Other Modules
  , module Data.Discrimination.Combinators
  , module Data.Discrimination.Table
  ) where

import Data.Discrimination.Class
import Data.Discrimination.Combinators
import Data.Discrimination.Join
import Data.Discrimination.Table
import Data.Discrimination.Type
