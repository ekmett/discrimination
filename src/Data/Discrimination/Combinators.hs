-- | Useful combinators for generalized list comprehensions.
module Data.Discrimination.Combinators
  ( nub, nubWith
  , sort, sortWith
  , group, groupWith
  ) where

import Control.Applicative
import Data.Discrimination.Class
import Data.Discrimination.Type

-- | Similar to 'Data.List.group', except we do not require groups to be clustered.
--
-- This combinator still operates in linear time.
--
-- @
-- 'group' = 'groupWith' 'id'
-- @
group :: Disorder a => [a] -> [[a]]
group as = disorder % [(a, a) | a <- as]

-- | Replaces 'GHC.Exts.groupWith' using discrimination. The result is not sorted.
groupWith :: Disorder b => (a -> b) -> [a] -> [[a]]
groupWith f as = disorder % [(f a, a) | a <- as]

-- | This linear time replacement for 'Data.List.nub' from @Data.List@ uses discrimination.
--
-- @
-- 'nub' = 'nubWith' 'id'
-- 'nub' as = 'head' 'Control.Applicative.<$>' 'group' as
-- @
nub :: Disorder a => [a] -> [a]
nub as = head <$> group as

-- |
-- @
-- 'nubWith' f as = 'head' 'Control.Applicative.<$>' 'groupWith' f as
-- @
nubWith :: Disorder b => (a -> b) -> [a] -> [a]
nubWith f as = head <$> groupWith f as

-- | This linear time replacement for 'Data.List.sort' uses discrimination.
--
-- @
-- 'sort' = 'sortWith' 'id'
-- @
sort :: Order a => [a] -> [a]
sort as = concat $ order % [ (a,a) | a <- as ]

-- | This linear time replacement for 'GHC.Exts.sortWith' and 'Data.List.sortOn' uses discrimination.
sortWith :: Order b => (a -> b) -> [a] -> [a]
sortWith f as = concat $ order % [ (f a, a) | a <- as ]
