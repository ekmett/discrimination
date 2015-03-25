{-# LANGUAGE CPP, GADTs, TypeOperators, RankNTypes, DeriveDataTypeable, DefaultSignatures, FlexibleContexts, TupleSections, ParallelListComp #-}
{-# OPTIONS_GHC -fno-cse -fno-full-laziness #-}

module Data.Discrimination.Class
  ( -- * Unordered Discrimination
    Grouping(..)
  , Grouping1(..)
  , groupingBag
  , groupingSet
  , groupingEq
    -- * Ordered Discrimination
  , Sorting(..)
  , Sorting1(..)
  , sortingBag
  , sortingSet
  , sortingCompare
  ) where

import Control.Monad ((<=<), join)
import Data.Bits
import Data.Complex
import Data.Ratio
import Data.Discrimination.Type
import Data.Foldable hiding (concat)
import Data.Functor
import Data.Functor.Compose
import Data.Functor.Contravariant
import Data.Functor.Contravariant.Divisible
import Data.Functor.Contravariant.Generic
import Data.Int
import Data.List as List
import Data.Proxy
import Data.Void
import Data.Word
import Prelude hiding (read)

--------------------------------------------------------------------------------
-- * Unordered Discrimination (for partitioning)
--------------------------------------------------------------------------------

-- | 'Eq' equipped with a compatible stable unordered discriminator.
class Grouping a where
  grouping :: Disc a
#ifndef HLINT
  default grouping :: Deciding Grouping a => Disc a
  grouping = deciding (Proxy :: Proxy Grouping) grouping
#endif

instance Grouping Void where
  grouping = lose id

instance Grouping Word8 where
  grouping = contramap fromIntegral groupingShort

instance Grouping Word16 where
  grouping = contramap fromIntegral groupingShort

instance Grouping Word32 where
  grouping = Disc (runs <=< runDisc groupingShort . join . runDisc groupingShort . map radices) where
    radices (x,b) = (fromIntegral x .&. 0xffff, (fromIntegral (unsafeShiftR x 16), (x,b)))

instance Grouping Word64 where
  grouping = Disc (runs <=< runDisc groupingShort . join . runDisc groupingShort . join
                          . runDisc groupingShort . join . runDisc groupingShort . map radices)
    where
      radices (x,b) = (fromIntegral x .&. 0xffff, (fromIntegral (unsafeShiftR x 16) .&. 0xffff
                    , (fromIntegral (unsafeShiftR x 32) .&. 0xffff, (fromIntegral (unsafeShiftR x 48)
                    , (x,b)))))


instance Grouping Word where
  grouping
    | (maxBound :: Word) == 4294967295 = contramap (fromIntegral :: Word -> Word32) grouping
    | otherwise                        = contramap (fromIntegral :: Word -> Word64) grouping

instance Grouping Int8 where
  grouping = contramap (\x -> fromIntegral x + 128) groupingShort

instance Grouping Int16 where
  grouping = contramap (\x -> fromIntegral x + 32768) groupingShort

instance Grouping Int32 where
  grouping = contramap (\x -> fromIntegral (x - minBound) :: Word32) grouping

instance Grouping Int64 where
  grouping = contramap (\x -> fromIntegral (x - minBound) :: Word64) grouping

instance Grouping Int where
  grouping = contramap (\x -> fromIntegral (x - minBound) :: Word) grouping

instance Grouping Bool
instance (Grouping a, Grouping b) => Grouping (a, b)
instance (Grouping a, Grouping b, Grouping c) => Grouping (a, b, c)
instance (Grouping a, Grouping b, Grouping c, Grouping d) => Grouping (a, b, c, d)
instance Grouping a => Grouping [a]
instance Grouping a => Grouping (Maybe a)
instance (Grouping a, Grouping b) => Grouping (Either a b)
instance Grouping a => Grouping (Complex a) where
  grouping = divide (\(a :+ b) -> (a, b)) grouping grouping
instance (Grouping a, Integral a) => Grouping (Ratio a) where
  grouping = divide (\r -> (numerator r, denominator r)) grouping grouping
instance (Grouping1 f, Grouping1 g, Grouping a) => Grouping (Compose f g a) where
  grouping = getCompose `contramap` grouping1 (grouping1 grouping)

class Grouping1 f where
  grouping1 :: Disc a -> Disc (f a)
#ifndef HLINT
  default grouping1 :: Deciding1 Grouping f => Disc a -> Disc (f a)
  grouping1 = deciding1 (Proxy :: Proxy Grouping) grouping
#endif

instance Grouping1 []
instance Grouping1 Maybe
instance Grouping a => Grouping1 (Either a)
instance Grouping a => Grouping1 ((,) a)
instance (Grouping a, Grouping b) => Grouping1 ((,,) a b)
instance (Grouping a, Grouping b, Grouping c) => Grouping1 ((,,,) a b c)
instance (Grouping1 f, Grouping1 g) => Grouping1 (Compose f g) where
  grouping1 f = getCompose `contramap` grouping1 (grouping1 f)
instance Grouping1 Complex where
  grouping1 f = divide (\(a :+ b) -> (a, b)) f f

-- | Valid definition for @('==')@ in terms of 'Grouping'.
groupingEq :: Grouping a => a -> a -> Bool
groupingEq a b = case runDisc grouping [(a,()),(b,())] of
  _:_:_ -> False
  _ -> True
{-# INLINE groupingEq #-}

--------------------------------------------------------------------------------
-- * Ordered Discrimination
--------------------------------------------------------------------------------

-- | 'Ord' equipped with a compatible stable, ordered discriminator.
class Grouping a => Sorting a where
  sorting :: Disc a
#ifndef HLINT
  default sorting :: Deciding Sorting a => Disc a
  sorting = deciding (Proxy :: Proxy Sorting) sorting
#endif

instance Sorting Word8 where
  sorting = contramap fromIntegral (sortingNat 256)

instance Sorting Word16 where
  sorting = contramap fromIntegral (sortingNat 65536)

instance Sorting Word32 where
  -- sorting = divide (\x -> ((fromIntegral (shiftR x 48) .&. 0xffff, fromIntegral (shiftR x 32) .&. 0xffff), (fromIntegral (unsafeShiftR x 16) .&. 0xffff, fromIntegral x .&. 0xffff))) go go where go = divide id (sortingNat 65536) (sortingNat 65536)
  sorting = Disc (runs <=< runDisc (sortingNat 65536) . join . runDisc (sortingNat 65536) . map radices) where
    radices (x,b) = (fromIntegral x .&. 0xffff, (fromIntegral (unsafeShiftR x 16), (x,b)))


instance Sorting Word64 where
  -- sorting = divide (\x -> ((fromIntegral (shiftR x 48) .&. 0xffff, fromIntegral (shiftR x 32) .&. 0xffff), (fromIntegral (unsafeShiftR x 16) .&. 0xffff, fromIntegral x .&. 0xffff))) go go where go = divide id (sortingNat 65536) (sortingNat 65536)
  sorting = Disc (runs <=< runDisc (sortingNat 65536) . join . runDisc (sortingNat 65536) . join
                         . runDisc (sortingNat 65536) . join . runDisc (sortingNat 65536) . map radices)
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

-- TODO: Integer and Natural?

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
  sorting1 :: Disc a -> Disc (f a)
#ifndef HLINT
  default sorting1 :: Deciding1 Sorting f => Disc a -> Disc (f a)
  sorting1 = deciding1 (Proxy :: Proxy Sorting) sorting
#endif

instance (Sorting1 f, Sorting1 g) => Sorting1 (Compose f g) where
  sorting1 f = getCompose `contramap` sorting1 (sorting1 f)

instance Sorting1 []
instance Sorting1 Maybe
instance Sorting a => Sorting1 (Either a)

-- | Valid definition for 'compare' in terms of 'Sorting'.
sortingCompare :: Sorting a => a -> a -> Ordering
sortingCompare a b = case runDisc sorting [(a,LT),(b,GT)] of
  [r]:_ -> r
  _     -> EQ
{-# INLINE sortingCompare #-}

--------------------------------------------------------------------------------
-- * Collections
--------------------------------------------------------------------------------

-- | Construct a stable ordered discriminator that sorts a list as multisets of elements from another stable ordered discriminator.
--
-- The resulting discriminator only cares about the set of keys and their multiplicity, and is sorted as if we'd
-- sorted each key in turn before comparing.
sortingBag :: Foldable f => Disc k -> Disc (f k)
sortingBag = sortingColl updateBag

-- | Construct a stable ordered discriminator that sorts a list as sets of elements from another stable ordered discriminator.
--
-- The resulting discriminator only cares about the set of keys, and is sorted as if we'd
-- sorted each key in turn before comparing.
sortingSet :: Foldable f => Disc k -> Disc (f k)
sortingSet = sortingColl updateSet

-- | Construct an stable unordered discriminator that partitions into equivalence classes based on the equivalence of keys as a multiset.
groupingBag :: Foldable f => Disc k -> Disc (f k)
groupingBag = groupingColl updateBag

-- | Construct an stable unordered discriminator that partitions into equivalence classes based on the equivalence of keys as a set.
groupingSet :: Foldable f => Disc k -> Disc (f k)
groupingSet = groupingColl updateSet

--------------------------------------------------------------------------------
-- * Utilities
--------------------------------------------------------------------------------

runs :: Eq a => [(a,b)] -> [[b]]
runs [] = []
runs ((a,b):xs0) = (b:ys0) : runs zs0
  where
    (ys0,zs0) = go xs0
    go [] = ([],[])
    go xs@((a', b'):xs')
      | a == a' = case go xs' of
         (ys, zs) -> (b':ys,zs)
      | otherwise = ([], xs)

sortingColl :: Foldable f => ([Int] -> Int -> [Int]) -> Disc k -> Disc (f k)
sortingColl update r = Disc $ \xss -> let
    (kss, vs)           = unzip xss
    elemKeyNumAssocs    = groupNum (toList <$> kss)
    keyNumBlocks        = runDisc r elemKeyNumAssocs
    keyNumElemNumAssocs = groupNum keyNumBlocks
    sigs                = bdiscNat (length kss) update keyNumElemNumAssocs
    yss                 = zip sigs vs
  in filter (not . null) $ sorting1 (sortingNat (length keyNumBlocks)) `runDisc` yss

groupNum :: [[k]] -> [(k,Int)]
groupNum kss = concat [ (,n) <$> ks | n <- [0..] | ks <- kss ]

groupingColl :: Foldable f => ([Int] -> Int -> [Int]) -> Disc k -> Disc (f k)
groupingColl update r = Disc $ \xss -> let
    (kss, vs)           = unzip xss
    elemKeyNumAssocs    = groupNum (toList <$> kss)
    keyNumBlocks        = runDisc r elemKeyNumAssocs
    keyNumElemNumAssocs = groupNum keyNumBlocks
    sigs                = bdiscNat (length kss) update keyNumElemNumAssocs
    yss                 = zip sigs vs
  in filter (not . null) $ grouping1 (groupingNat (length keyNumBlocks)) `runDisc` yss

updateBag :: [Int] -> Int -> [Int]
updateBag vs v = v : vs

updateSet :: [Int] -> Int -> [Int]
updateSet [] w = [w]
updateSet vs@(v:_) w
  | v == w    = vs
  | otherwise = w : vs
