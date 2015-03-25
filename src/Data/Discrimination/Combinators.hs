module Data.Discrimination.Combinators
  ( 
  -- * Common
  -- $common
    nub, nubWith
  , sort, sortWith
  , group, groupWith
  -- * Container Construction
  , toMap, toMapWith, toMapWithKey
  , toIntMap, toIntMapWith, toIntMapWithKey
  , toSet
  , toIntSet
  -- * Joins
  , joining, inner, outer, leftOuter, rightOuter
  ) where

import Control.Applicative
import Control.Arrow
import Data.Discrimination.Class
import Data.Discrimination.Type
import Data.IntMap.Lazy as IntMap
import Data.IntSet as IntSet
import Data.Map as Map
import Data.Maybe (catMaybes)
import Data.Set as Set

--------------------------------------------------------------------------------
-- * Common
--------------------------------------------------------------------------------

-- $common
-- Useful combinators for generalized list comprehensions.

-- | /O(n)/. Similar to 'Data.List.group', except we do not require groups to be clustered.
--
-- This combinator still operates in linear time, at the expense of productivity.
--
-- The result equivalence classes are _not_ sorted, but the grouping is stable.
--
-- @
-- 'group' = 'groupWith' 'id'
-- @
group :: Grouping a => [a] -> [[a]]
group as = runDisc grouping [(a, a) | a <- as]

-- | /O(n)/. This is a replacement for 'GHC.Exts.groupWith' using discrimination.
--
-- The result equivalence classes are _not_ sorted, but the grouping is stable.
groupWith :: Grouping b => (a -> b) -> [a] -> [[a]]
groupWith f as = runDisc grouping [(f a, a) | a <- as]

-- | /O(n)/. This upgrades 'Data.List.nub' from @Data.List@ from /O(n^2)/ to /O(n)/ by using
-- unordered discrimination.
--
-- @
-- 'nub' = 'nubWith' 'id'
-- 'nub' as = 'head' 'Control.Applicative.<$>' 'group' as
-- @
nub :: Grouping a => [a] -> [a]
nub as = head <$> group as

-- | /O(n)/. 'nub' with a Schwartzian transform.
--
-- @
-- 'nubWith' f as = 'head' 'Control.Applicative.<$>' 'groupWith' f as
-- @
nubWith :: Grouping b => (a -> b) -> [a] -> [a]
nubWith f as = head <$> groupWith f as

-- | / O(n)/. Sort a list using discrimination.
--
-- @
-- 'sort' = 'sortWith' 'id'
-- @
sort :: Sorting a => [a] -> [a]
sort as = concat $ runDisc sorting [ (a,a) | a <- as ]

-- | /O(n)/. Sort a list with a Schwartzian transformation by using discrimination. 
--
-- This linear time replacement for 'GHC.Exts.sortWith' and 'Data.List.sortOn' uses discrimination.
sortWith :: Sorting b => (a -> b) -> [a] -> [a]
sortWith f as = concat $ runDisc sorting [ (f a, a) | a <- as ]

--------------------------------------------------------------------------------
-- * Container Construction
--------------------------------------------------------------------------------

-- | /O(n)/. Construct a 'Map'.
--
-- This is an asymptotically faster version of 'Data.Map.fromList', which exploits ordered discrimination.
--
-- >>> toMap [] == empty
-- True
--
-- >>> toMap [(5,"a"), (3 :: Int,"b"), (5, "c")]
-- fromList [(5,"c"), (3,"b")]
--
-- >>> toMap [(5,"c"), (3,"b"), (5 :: Int, "a")]
-- fromList [(5,"a"), (3,"b")]
toMap :: Sorting k => [(k, v)] -> Map k v
toMap kvs = Map.fromDistinctAscList $ last <$> runDisc sorting [ (fst kv, kv) | kv <- kvs ]

-- | /O(n)/. Construct a 'Map', combining values.
--
-- This is an asymptotically faster version of 'Data.Map.fromListWith', which exploits ordered discrimination.
--
-- (Note: values combine in anti-stable order for compatibility with 'Data.Map.fromListWith')
--
-- >>> toMapWith (++) [(5,"a"), (5,"b"), (3,"b"), (3,"a"), (5 :: Int,"c")]
-- fromList [(3, "ab"), (5, "cba")]
--
-- >>> toMapWith (++) [] == empty
-- True
toMapWith :: Sorting k => (v -> v -> v) -> [(k, v)] -> Map k v
toMapWith f kvs0 = Map.fromDistinctAscList $ go <$> runDisc sorting [ (fst kv, kv) | kv <- kvs0 ] where
  go ((k,v):kvs) = (k, Prelude.foldl (flip (f . snd)) v kvs)
  go []          = error "bad sort"

-- | /O(n)/. Construct a 'Map', combining values with access to the key.
--
-- This is an asymptotically faster version of 'Data.Map.fromListWithKey', which exploits ordered discrimination.
--
-- (Note: the values combine in anti-stable order for compatibility with 'Data.Map.fromListWithKey')
--
-- >>> let f key new_value old_value = show key ++ ":" ++ new_value ++ "|" ++ old_value
-- >>> toMapWithKey f [(5,"a"), (5,"b"), (3,"b"), (3,"a"), (5 :: Int,"c")]
-- fromList [(3, "3:a|b"), (5, "5:c|5:b|a")]
--
-- >>> toMapWithKey f [] == empty
-- True
toMapWithKey :: Sorting k => (k -> v -> v -> v) -> [(k, v)] -> Map k v
toMapWithKey f kvs0 = Map.fromDistinctAscList $ go <$> runDisc sorting [ (fst kv, kv) | kv <- kvs0 ] where
  go ((k,v):kvs) = (k, Prelude.foldl (flip (f k . snd)) v kvs)
  go []          = error "bad sort"

-- | /O(n)/. Construct an 'IntMap'.
--
-- >>> toIntMap [] == empty
-- True
--
-- >>> toIntMap [(5,"a"), (3,"b"), (5, "c")]
-- fromList [(5,"c"), (3,"b")]
--
-- >>> toIntMap [(5,"c"), (3,"b"), (5, "a")]
-- fromList [(5,"a"), (3,"b")]
toIntMap :: [(Int, v)] -> IntMap v
toIntMap kvs = IntMap.fromDistinctAscList $ last <$> runDisc sorting [ (fst kv, kv) | kv <- kvs ]

-- | /O(n)/. Construct an 'IntMap', combining values.
--
-- This is an asymptotically faster version of 'Data.IntMap.Lazy.fromListWith', which exploits ordered discrimination.
--
-- (Note: values combine in anti-stable order for compatibility with 'Data.IntMap.Lazy.fromListWith')
--
-- >>> toIntMapWith (++) [(5,"a"), (5,"b"), (3,"b"), (3,"a"), (5,"c")]
-- fromList [(3, "ab"), (5, "cba")]
--
-- >>> toIntMapWith (++) [] == empty
-- True
toIntMapWith :: (v -> v -> v) -> [(Int, v)] -> IntMap v
toIntMapWith f kvs0 = IntMap.fromDistinctAscList $ go <$> runDisc sorting [ (fst kv, kv) | kv <- kvs0 ] where
  go ((k,v):kvs) = (k, Prelude.foldl (flip (f . snd)) v kvs)
  go []          = error "bad sort"

-- | /O(n)/. Construct a 'Map', combining values with access to the key.
--
-- This is an asymptotically faster version of 'Data.IntMap.Lazy.fromListWithKey', which exploits ordered discrimination.
--
-- (Note: the values combine in anti-stable order for compatibility with 'Data.IntMap.Lazy.fromListWithKey')
--
-- >>> let f key new_value old_value = show key ++ ":" ++ new_value ++ "|" ++ old_value
-- >>> toIntMapWithKey f [(5,"a"), (5,"b"), (3,"b"), (3,"a"), (5,"c")]
-- fromList [(3, "3:a|b"), (5, "5:c|5:b|a")]
--
-- >>> toIntMapWithKey f [] == empty
-- True
toIntMapWithKey :: (Int -> v -> v -> v) -> [(Int, v)] -> IntMap v
toIntMapWithKey f kvs0 = IntMap.fromDistinctAscList $ go <$> runDisc sorting [ (fst kv, kv) | kv <- kvs0 ] where
  go ((k,v):kvs) = (k, Prelude.foldl (flip (f k . snd)) v kvs)
  go []          = error "bad sort"

-- | /O(n)/. Construct a 'Set' in linear time.
--
-- This is an asymptotically faster version of 'Data.Set.fromList', which exploits ordered discrimination.
toSet :: Sorting k => [k] -> Set k
toSet kvs = Set.fromDistinctAscList $ last <$> runDisc sorting [ (kv, kv) | kv <- kvs ]

-- | /O(n)/. Construct an 'IntSet' in linear time.
--
-- This is an asymptotically faster version of 'Data.IntSet.fromList', which exploits ordered discrimination.
toIntSet :: [Int] -> IntSet
toIntSet kvs = IntSet.fromDistinctAscList $ last <$> runDisc sorting [ (kv, kv) | kv <- kvs ]

--------------------------------------------------------------------------------
-- * Joins
--------------------------------------------------------------------------------

-- | Perform a full outer join while explicit merging of the two result tables a table at a time.
joining
  :: Disc d            -- ^ the discriminator to use
  -> ([a] -> [b] -> c) -- ^ how to join two tables
  -> (a -> d)          -- ^ selector for the left table
  -> (b -> d)          -- ^ selector for the right table
  -> [a]               -- ^ left table
  -> [b]               -- ^ right table
  -> [c]               
joining disc abc ad bd as bs = spanEither abc <$> runDisc disc (((ad &&& Left) <$> as) ++ ((bd &&& Right) <$> bs))
{-# INLINE joining #-}

-- | /O(n)/. Perform an inner join, with operations defined one row at a time.
--
-- The results are grouped by the discriminator.
--
-- This takes operation time linear in both the input and result sets.
inner
  :: Disc d        -- ^ the discriminator to use
  -> (a -> b -> c) -- ^ how to join two rows
  -> (a -> d)      -- ^ selector for the left table
  -> (b -> d)      -- ^ selector for the right table
  -> [a]           -- ^ left table
  -> [b]           -- ^ right table
  -> [[c]]
inner disc abc ad bd as bs = catMaybes $ joining disc go ad bd as bs where
  go ap bp
    | Prelude.null ap || Prelude.null bp = Nothing
    | otherwise = Just (liftA2 abc ap bp)

-- | /O(n)/. Perform a full outer join with operations defined one row at a time.
--
-- The results are grouped by the discriminator.
--
-- This takes operation time linear in both the input and result sets.
outer
  :: Disc d        -- ^ the discriminator to use
  -> (a -> b -> c) -- ^ how to join two rows
  -> (a -> c)      -- ^ row present on the left, missing on the right
  -> (b -> c)      -- ^ row present on the right, missing on the left
  -> (a -> d)      -- ^ selector for the left table
  -> (b -> d)      -- ^ selector for the right table
  -> [a]           -- ^ left table
  -> [b]           -- ^ right table
  -> [[c]]
outer disc abc ac bc ad bd as bs = joining disc go ad bd as bs where
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
  :: Disc d        -- ^ the discriminator to use
  -> (a -> b -> c) -- ^ how to join two rows
  -> (a -> c)      -- ^ row present on the left, missing on the right
  -> (a -> d)      -- ^ selector for the left table
  -> (b -> d)      -- ^ selector for the right table
  -> [a]           -- ^ left table
  -> [b]           -- ^ right table
  -> [[c]]
leftOuter disc abc ac ad bd as bs = catMaybes $ joining disc go ad bd as bs where
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
  :: Disc d        -- ^ the discriminator to use
  -> (a -> b -> c) -- ^ how to join two rows
  -> (b -> c)      -- ^ row present on the right, missing on the left
  -> (a -> d)      -- ^ selector for the left table
  -> (b -> d)      -- ^ selector for the right table
  -> [a]           -- ^ left table
  -> [b]           -- ^ right table
  -> [[c]]
rightOuter disc abc bc ad bd as bs = catMaybes $ joining disc go ad bd as bs where
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

