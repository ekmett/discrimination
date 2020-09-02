{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# OPTIONS_GHC -fno-cse -fno-full-laziness #-}
module Data.Discrimination.Sorting
  ( Sort(..)
  -- * Sorting
  , Sorting(..)
  , Sorting1(..)
  -- * Combinators
  -- $common
  , sort, sortWith, desc
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
  -- * Internals
  , sortingNat
  , sortingBag
  , sortingSet
  ) where

import Control.Applicative
import Control.Arrow
import Control.Monad
import Data.Bits
import Data.Discrimination.Grouping
import Data.Discrimination.Internal
import Data.Foldable as Foldable hiding (concat)
import Data.Functor.Compose
import Data.Functor.Contravariant
import Data.Functor.Contravariant.Divisible
import Data.Functor.Contravariant.Generic
import Data.Int
import Data.IntMap.Lazy as IntMap
import Data.IntSet as IntSet
import qualified Data.List as List
import Data.Map as Map
import Data.Proxy
import Data.Semigroup hiding (Any)
import Data.Set as Set
import Data.Typeable
import Data.Void
import Data.Word
import GHC.Integer.GMP.Internals
import GHC.Natural
import GHC.Word
import GHC.Int
import Prelude hiding (read, concat)

--------------------------------------------------------------------------------
-- * Common
--------------------------------------------------------------------------------


-- | Stable Ordered Discriminator

-- TODO: use [(a,b)] -> [NonEmpty b] to better indicate safety?
newtype Sort a = Sort { runSort :: forall b. [(a,b)] -> [[b]] }
  deriving Typeable

mkSort :: (forall b. [(a, b)] -> [[b]]) -> Sort a
mkSort f = Sort $ \xs -> case xs of
  []       -> []
  [(_, v)] -> [[v]]
  _        -> f xs

#ifndef HLINT
type role Sort representational
#endif

instance Contravariant Sort where
  contramap f (Sort g) = Sort $ g . fmap (first f)

instance Divisible Sort where
  conquer = mkSort $ return . fmap snd
  divide k (Sort l) (Sort r) = Sort $ \xs ->
    l [ (b, (c, d)) | (a,d) <- xs, let (b, c) = k a] >>= r

instance Decidable Sort where
  lose k = Sort $ fmap (absurd.k.fst)
  choose f (Sort l) (Sort r) = mkSort $ \xs -> let
      ys = fmap (first f) xs
    in l [ (k,v) | (Left k, v) <- ys]
    ++ r [ (k,v) | (Right k, v) <- ys]

instance Semigroup (Sort a) where
  Sort l <> Sort r = Sort $ \xs -> l [ (fst x, x) | x <- xs ] >>= r

instance Monoid (Sort a) where
  mempty = conquer
  mappend = (<>)

--------------------------------------------------------------------------------
-- * Ordered Discrimination
--------------------------------------------------------------------------------

-- | 'Ord' equipped with a compatible stable, ordered discriminator.
class Grouping a => Sorting a where
  -- | For every strictly monotone-increasing function @f@:
  --
  -- @
  -- 'contramap' f 'sorting' ≡ 'sorting'
  -- @
  sorting :: Sort a
#ifndef HLINT
  default sorting :: Deciding Sorting a => Sort a
  sorting = deciding (Proxy :: Proxy Sorting) sorting
#endif

instance Sorting () where
  sorting = conquer

instance Sorting Integer where
  sorting = choose cases (desc sorting) (choose id sorting sorting) where
    cases :: Integer -> Either (GmpSize,[GmpLimb]) (Either Int (GmpSize,[GmpLimb]))
    cases (Jn# b) = Left          $ decomposeBigNat b
    cases (S# i#) = Right . Left  $ I# i#
    cases (Jp# b) = Right . Right $ decomposeBigNat b

instance Sorting Natural where
  sorting = choose cases sorting sorting where
    cases :: Natural -> Either GmpLimb (GmpSize,[GmpLimb])
    cases (NatS# w#) = Left  $ W# w#
    cases (NatJ# b)  = Right $ decomposeBigNat b

instance Sorting Word8 where
  sorting = contramap fromIntegral (sortingNat 256)

instance Sorting Word16 where
  sorting = contramap fromIntegral (sortingNat 65536)

instance Sorting Word32 where
  sorting = Sort (runs <=< runSort (sortingNat 65536) . join . runSort (sortingNat 65536) . fmap radices) where
    radices (x,b) = (fromIntegral x .&. 0xffff, (fromIntegral (unsafeShiftR x 16), (x,b)))


instance Sorting Word64 where
  sorting = Sort (runs <=< runSort (sortingNat 65536) . join . runSort (sortingNat 65536) . join
                         . runSort (sortingNat 65536) . join . runSort (sortingNat 65536) . fmap radices)
    where
      radices (x,b) = (fromIntegral x .&. 0xffff, (fromIntegral (unsafeShiftR x 16) .&. 0xffff
                    , (fromIntegral (unsafeShiftR x 32) .&. 0xffff, (fromIntegral (unsafeShiftR x 48)
                    , (x,b)))))


instance Sorting Word where
  sorting
    | (maxBound :: Word) == 4294967295 = contramap (fromIntegral :: Word -> Word32) sorting
    | otherwise                        = contramap (fromIntegral :: Word -> Word64) sorting

instance Sorting Int8 where
  sorting = contramap (\x -> fromIntegral (x - minBound)) (sortingNat 256)

instance Sorting Int16 where
  sorting = contramap (\x -> fromIntegral (x - minBound)) (sortingNat 65536)

instance Sorting Int32 where
  sorting = contramap (\x -> fromIntegral (x - minBound) :: Word32) sorting

instance Sorting Int64 where
  sorting = contramap (\x -> fromIntegral (x - minBound) :: Word64) sorting

instance Sorting Int where
  sorting = contramap (\x -> fromIntegral (x - minBound) :: Word) sorting

instance Sorting Char where
  sorting = Sort (runs <=< runSort (sortingNat 1087) . join . runSort (sortingNat 1024) . fmap radices) where
    radices (c,b) = (x .&. 0x3ff, (unsafeShiftR x 10, (x,b))) where
      x = fromEnum c

instance Sorting Void
instance Sorting Bool
instance Sorting a => Sorting [a]
instance Sorting a => Sorting (Maybe a)
instance (Sorting a, Sorting b) => Sorting (Either a b)
instance (Sorting a, Sorting b) => Sorting (a, b)
instance (Sorting a, Sorting b, Sorting c) => Sorting (a, b, c)
instance (Sorting a, Sorting b, Sorting c, Sorting d) => Sorting (a, b, c, d)
instance (Sorting1 f, Sorting1 g, Sorting a) => Sorting (Compose f g a) where
  sorting = getCompose `contramap` sorting1 (sorting1 sorting)

class Grouping1 f => Sorting1 f  where
  sorting1 :: Sort a -> Sort (f a)
#ifndef HLINT
  default sorting1 :: Deciding1 Sorting f => Sort a -> Sort (f a)
  sorting1 = deciding1 (Proxy :: Proxy Sorting) sorting
#endif

instance (Sorting1 f, Sorting1 g) => Sorting1 (Compose f g) where
  sorting1 f = getCompose `contramap` sorting1 (sorting1 f)

instance Sorting1 []
instance Sorting1 Maybe
instance Sorting a => Sorting1 (Either a)

-- | Valid definition for 'compare' in terms of 'Sorting'.
sortingCompare :: Sorting a => a -> a -> Ordering
sortingCompare a b = case runSort sorting [(a,LT),(b,GT)] of
  [r]:_ -> r
  _     -> EQ
{-# INLINE sortingCompare #-}

--------------------------------------------------------------------------------
-- * Utilities
--------------------------------------------------------------------------------

sortingNat :: Int -> Sort Int
sortingNat n = mkSort $ \xs -> List.filter (not . List.null) (bdiscNat n upd xs) where
  upd vs v = v : vs
{-# INLINE sortingNat #-}

--------------------------------------------------------------------------------
-- * Collections
--------------------------------------------------------------------------------

-- | Construct a stable ordered discriminator that sorts a list as multisets of elements from another stable ordered discriminator.
--
-- The resulting discriminator only cares about the set of keys and their multiplicity, and is sorted as if we'd
-- sorted each key in turn before comparing.
sortingBag :: Foldable f => Sort k -> Sort (f k)
sortingBag = sortingColl updateBag

-- | Construct a stable ordered discriminator that sorts a list as sets of elements from another stable ordered discriminator.
--
-- The resulting discriminator only cares about the set of keys, and is sorted as if we'd
-- sorted each key in turn before comparing.
sortingSet :: Foldable f => Sort k -> Sort (f k)
sortingSet = sortingColl updateSet

sortingColl :: Foldable f => ([Int] -> Int -> [Int]) -> Sort k -> Sort (f k)
sortingColl upd r = Sort $ \xss -> let
    (kss, vs)           = unzip xss
    elemKeyNumAssocs    = groupNum (Foldable.toList <$> kss)
    keyNumBlocks        = runSort r elemKeyNumAssocs
    keyNumElemNumAssocs = groupNum keyNumBlocks
    sigs                = bdiscNat (length kss) upd keyNumElemNumAssocs
    yss                 = zip sigs vs
  in List.filter (not . List.null) $ sorting1 (sortingNat (length keyNumBlocks)) `runSort` yss

--------------------------------------------------------------------------------
-- * Combinators
--------------------------------------------------------------------------------

desc :: Sort a -> Sort a
desc (Sort l) = Sort (reverse . l)

-- $common
-- Useful combinators.

-- | / O(n)/. Sort a list using discrimination.
--
-- @
-- 'sort' = 'sortWith' 'id'
-- @
sort :: Sorting a => [a] -> [a]
sort as = List.concat $ runSort sorting [ (a,a) | a <- as ]

-- | /O(n)/. Sort a list with a Schwartzian transformation by using discrimination.
--
-- This linear time replacement for 'GHC.Exts.sortWith' and 'Data.List.sortOn' uses discrimination.
sortWith :: Sorting b => (a -> b) -> [a] -> [a]
sortWith f as = List.concat $ runSort sorting [ (f a, a) | a <- as ]

--------------------------------------------------------------------------------
-- * Containers
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
toMap kvs = Map.fromDistinctAscList $ last <$> runSort sorting [ (fst kv, kv) | kv <- kvs ]

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
toMapWith f kvs0 = Map.fromDistinctAscList $ go <$> runSort sorting [ (fst kv, kv) | kv <- kvs0 ] where
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
toMapWithKey f kvs0 = Map.fromDistinctAscList $ go <$> runSort sorting [ (fst kv, kv) | kv <- kvs0 ] where
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
toIntMap kvs = IntMap.fromDistinctAscList $ last <$> runSort sorting [ (fst kv, kv) | kv <- kvs ]

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
toIntMapWith f kvs0 = IntMap.fromDistinctAscList $ go <$> runSort sorting [ (fst kv, kv) | kv <- kvs0 ] where
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
toIntMapWithKey f kvs0 = IntMap.fromDistinctAscList $ go <$> runSort sorting [ (fst kv, kv) | kv <- kvs0 ] where
  go ((k,v):kvs) = (k, Prelude.foldl (flip (f k . snd)) v kvs)
  go []          = error "bad sort"

-- | /O(n)/. Construct a 'Set' in linear time.
--
-- This is an asymptotically faster version of 'Data.Set.fromList', which exploits ordered discrimination.
toSet :: Sorting k => [k] -> Set k
toSet kvs = Set.fromDistinctAscList $ last <$> runSort sorting [ (kv, kv) | kv <- kvs ]

-- | /O(n)/. Construct an 'IntSet' in linear time.
--
-- This is an asymptotically faster version of 'Data.IntSet.fromList', which exploits ordered discrimination.
toIntSet :: [Int] -> IntSet
toIntSet kvs = IntSet.fromDistinctAscList $ last <$> runSort sorting [ (kv, kv) | kv <- kvs ]
