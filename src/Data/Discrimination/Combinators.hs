-- | Useful combinators for generalized list comprehensions.
--
module Data.Discrimination.Combinators
  ( nub, nubWith
  , sort, sortWith
  , group, groupWith
  , toMap, toMapWith, toMapWithKey
  , toIntMap, toIntMapWith, toIntMapWithKey
  , toSet
  , toIntSet
  ) where

import Control.Applicative
import Data.Discrimination.Class
import Data.Discrimination.Type
import Data.IntMap.Lazy as IntMap
import Data.IntSet as IntSet
import Data.Map as Map
import Data.Set as Set

-- $setup
-- >>> import Data.Int

-- | Similar to 'Data.List.group', except we do not require groups to be clustered.
--
-- This combinator still operates in linear time, at the expense of productivity.
--
-- @
-- 'group' = 'groupWith' 'id'
-- @
group :: Grouping a => [a] -> [[a]]
group as = runDisc grouping [(a, a) | a <- as]

-- | Replaces 'GHC.Exts.groupWith' using discrimination. The result is not sorted.
groupWith :: Grouping b => (a -> b) -> [a] -> [[a]]
groupWith f as = runDisc grouping [(f a, a) | a <- as]

-- | This linear time replacement for 'Data.List.nub' from @Data.List@ uses discrimination.
--
-- @
-- 'nub' = 'nubWith' 'id'
-- 'nub' as = 'head' 'Control.Applicative.<$>' 'group' as
-- @
nub :: Grouping a => [a] -> [a]
nub as = head <$> group as

-- |
-- @
-- 'nubWith' f as = 'head' 'Control.Applicative.<$>' 'groupWith' f as
-- @
nubWith :: Grouping b => (a -> b) -> [a] -> [a]
nubWith f as = head <$> groupWith f as

-- | This linear time replacement for 'Data.List.sort' uses discrimination.
--
-- @
-- 'sort' = 'sortWith' 'id'
-- @
sort :: Sorting a => [a] -> [a]
sort as = concat $ runDisc sorting [ (a,a) | a <- as ]

-- | This linear time replacement for 'GHC.Exts.sortWith' and 'Data.List.sortOn' uses discrimination.
sortWith :: Sorting b => (a -> b) -> [a] -> [a]
sortWith f as = concat $ runDisc sorting [ (f a, a) | a <- as ]

-- | Construct a 'Map' in linear time.
--
-- This is an asymptotically faster version of 'Data.Map.fromList', which exploits ordered discrimination.
--
-- >>> toMap [] == empty
-- True
--
-- >>> toMap [(5,"a"), (3 :: Int16,"b"), (5, "c")]
-- fromList [(5,"c"), (3,"b")]
--
-- >>> toMap [(5,"c"), (3,"b"), (5 :: Int16, "a")]
-- fromList [(5,"a"), (3,"b")]
toMap :: Sorting k => [(k, v)] -> Map k v
toMap kvs = Map.fromDistinctAscList $ last <$> runDisc sorting [ (fst kv, kv) | kv <- kvs ]

-- | Construct a 'Map' in linear time, combining values.
--
-- This is an asymptotically faster version of 'Data.Map.fromListWith', which exploits ordered discrimination.
--
-- (Note: values combine in anti-stable order for compatibility with 'Data.Map.fromListWith')
--
-- >>> toMapWith (++) [(5,"a"), (5,"b"), (3,"b"), (3,"a"), (5 :: Int16,"c")]
-- fromList [(3, "ab"), (5, "cba")]
--
-- >>> toMapWith (++) [] == empty
-- True
toMapWith :: Sorting k => (v -> v -> v) -> [(k, v)] -> Map k v
toMapWith f kvs0 = Map.fromDistinctAscList $ go <$> runDisc sorting [ (fst kv, kv) | kv <- kvs0 ] where
  go ((k,v):kvs) = (k, Prelude.foldl (flip (f . snd)) v kvs)
  go []          = error "bad sort"

-- | Construct a 'Map' in linear time, combining values with access to the key.
--
-- This is an asymptotically faster version of 'Data.Map.fromListWithKey', which exploits ordered discrimination.
--
-- (Note: the values combine in anti-stable order for compatibility with 'Data.Map.fromListWithKey')
--
-- >>> let f key new_value old_value = show key ++ ":" ++ new_value ++ "|" ++ old_value
-- >>> toMapWithKey f [(5,"a"), (5,"b"), (3,"b"), (3,"a"), (5 :: Int16,"c")]
-- fromList [(3, "3:a|b"), (5, "5:c|5:b|a")]
--
-- >>> toMapWithKey f [] == empty
-- True
toMapWithKey :: Sorting k => (k -> v -> v -> v) -> [(k, v)] -> Map k v
toMapWithKey f kvs0 = Map.fromDistinctAscList $ go <$> runDisc sorting [ (fst kv, kv) | kv <- kvs0 ] where
  go ((k,v):kvs) = (k, Prelude.foldl (flip (f k . snd)) v kvs)
  go []          = error "bad sort"

-- | Construct an 'IntMap' in linear time.
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

-- | Construct an 'IntMap' in linear time, combining values.
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

-- | Construct a 'Map' in linear time, combining values with access to the key.
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

-- | Construct a 'Set' in linear time.
--
-- This is an asymptotically faster version of 'Data.Set.fromList', which exploits ordered discrimination.
toSet :: Sorting k => [k] -> Set k
toSet kvs = Set.fromDistinctAscList $ last <$> runDisc sorting [ (kv, kv) | kv <- kvs ]

-- | Construct an 'IntSet' in linear time.
--
-- This is an asymptotically faster version of 'Data.IntSet.fromList', which exploits ordered discrimination.
toIntSet :: [Int] -> IntSet
toIntSet kvs = IntSet.fromDistinctAscList $ last <$> runDisc sorting [ (kv, kv) | kv <- kvs ]
