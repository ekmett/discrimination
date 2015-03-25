module Data.Discrimination.Class
  ( Discriminating(..)
  -- * Joins
  , joining
  , inner
  , outer
  , leftOuter
  , rightOuter
  ) where

import Control.Applicative
import Control.Arrow
import Data.Functor.Contravariant.Divisible
import Data.Discrimination.Grouping
import Data.Discrimination.Sorting
import Data.Maybe (catMaybes)

class Decidable f => Discriminating f where
  disc :: f a -> [(a, b)] -> [[b]]

instance Discriminating Sort where
  disc = runSort

instance Discriminating Group where
  disc = runGroup

--------------------------------------------------------------------------------
-- * Joins
--------------------------------------------------------------------------------

-- | /O(n)/. Perform a full outer join while explicit merging of the two result tables a table at a time.
--
-- The results are grouped by the discriminator.
joining
  :: Discriminating f
  => f d            -- ^ the discriminator to use
  -> ([a] -> [b] -> c) -- ^ how to join two tables
  -> (a -> d)          -- ^ selector for the left table
  -> (b -> d)          -- ^ selector for the right table
  -> [a]               -- ^ left table
  -> [b]               -- ^ right table
  -> [c]               
joining m abc ad bd as bs = spanEither abc <$> disc m (((ad &&& Left) <$> as) ++ ((bd &&& Right) <$> bs))
{-# INLINE joining #-}

-- | /O(n)/. Perform an inner join, with operations defined one row at a time.
--
-- The results are grouped by the discriminator.
--
-- This takes operation time linear in both the input and result sets.
inner
  :: Discriminating f
  => f d           -- ^ the discriminator to use
  -> (a -> b -> c) -- ^ how to join two rows
  -> (a -> d)      -- ^ selector for the left table
  -> (b -> d)      -- ^ selector for the right table
  -> [a]           -- ^ left table
  -> [b]           -- ^ right table
  -> [[c]]
inner m abc ad bd as bs = catMaybes $ joining m go ad bd as bs where
  go ap bp
    | Prelude.null ap || Prelude.null bp = Nothing
    | otherwise = Just (liftA2 abc ap bp)

-- | /O(n)/. Perform a full outer join with operations defined one row at a time.
--
-- The results are grouped by the discriminator.
--
-- This takes operation time linear in both the input and result sets.
outer
  :: Discriminating f
  => f d           -- ^ the discriminator to use
  -> (a -> b -> c) -- ^ how to join two rows
  -> (a -> c)      -- ^ row present on the left, missing on the right
  -> (b -> c)      -- ^ row present on the right, missing on the left
  -> (a -> d)      -- ^ selector for the left table
  -> (b -> d)      -- ^ selector for the right table
  -> [a]           -- ^ left table
  -> [b]           -- ^ right table
  -> [[c]]
outer m abc ac bc ad bd as bs = joining m go ad bd as bs where
  go ap bp
    | Prelude.null ap = bc <$> bp
    | Prelude.null bp = ac <$> ap
    | otherwise = liftA2 abc ap bp

-- | /O(n)/. Perform a left outer join with operations defined one row at a time.
--
-- The results are grouped by the discriminator.
--
-- This takes operation time linear in both the input and result sets.
leftOuter
  :: Discriminating f
  => f d           -- ^ the discriminator to use
  -> (a -> b -> c) -- ^ how to join two rows
  -> (a -> c)      -- ^ row present on the left, missing on the right
  -> (a -> d)      -- ^ selector for the left table
  -> (b -> d)      -- ^ selector for the right table
  -> [a]           -- ^ left table
  -> [b]           -- ^ right table
  -> [[c]]
leftOuter m abc ac ad bd as bs = catMaybes $ joining m go ad bd as bs where
  go ap bp
    | Prelude.null ap = Nothing
    | Prelude.null bp = Just (ac <$> ap)
    | otherwise = Just (liftA2 abc ap bp)

-- | /O(n)/. Perform a right outer join with operations defined one row at a time.
--
-- The results are grouped by the discriminator.
--
-- This takes operation time linear in both the input and result sets.
rightOuter
  :: Discriminating f
  => f d        -- ^ the discriminator to use
  -> (a -> b -> c) -- ^ how to join two rows
  -> (b -> c)      -- ^ row present on the right, missing on the left
  -> (a -> d)      -- ^ selector for the left table
  -> (b -> d)      -- ^ selector for the right table
  -> [a]           -- ^ left table
  -> [b]           -- ^ right table
  -> [[c]]
rightOuter m abc bc ad bd as bs = catMaybes $ joining m go ad bd as bs where
  go ap bp
    | Prelude.null bp = Nothing
    | Prelude.null ap = Just (bc <$> bp)
    | otherwise = Just (liftA2 abc ap bp)

--------------------------------------------------------------------------------
-- * Unexported Utilities
--------------------------------------------------------------------------------

-- | Optimized and CPS'd version of 'Data.Either.partitionEithers', where all lefts are known to come before all rights
spanEither :: ([a] -> [b] -> c) -> [Either a b] -> c
spanEither k xs0 = go [] xs0 where
  go acc (Left x:xs) = go (x:acc) xs
  go acc rights = k (reverse acc) (fromRight <$> rights)
  fromRight (Right y) = y
  fromRight _ = error "spanEither: unstable"
