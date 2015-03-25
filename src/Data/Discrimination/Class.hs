{-# LANGUAGE GADTs, TypeOperators, RankNTypes, DeriveDataTypeable, DefaultSignatures, FlexibleContexts, TupleSections, ParallelListComp #-}
{-# OPTIONS_GHC -fno-cse -fno-full-laziness #-}

module Data.Discrimination.Class
  ( -- * Unordered Discrimination
    Grouping(..)
  , Grouping1(..)
  , discColl
  , discBag
  , discSet
    -- * Ordered Discrimination
  , Sorting(..)
  , Sorting1(..)
  , sdiscColl
  , sdiscBag
  , sdiscSet
  ) where

import Data.Bits
import Data.Complex
import Data.Ratio
import Data.Discrimination.Generic
import Data.Discrimination.Type
import Data.Foldable hiding (concat)
import Data.Functor
import Data.Functor.Compose
import Data.Functor.Contravariant
import Data.Functor.Contravariant.Divisible
import Data.Int
import Data.Proxy
import Data.Void
import Data.Word
import Prelude hiding (read)

-- * Unordered Discrimination (for partitioning)

-- | 'Eq' equipped with a compatible stable unordered discriminator.
class Grouping a where
  grouping :: Disc a
  default grouping :: Deciding Grouping a => Disc a
  grouping = deciding (Proxy :: Proxy Grouping) grouping

instance Grouping Void where
  grouping = lose id

instance Grouping Word8 where
  grouping = contramap fromIntegral discShort

instance Grouping Word16 where
  grouping = contramap fromIntegral discShort

instance Grouping Word32 where
  grouping = divide (\x -> (fromIntegral (unsafeShiftR x 16), fromIntegral x .&. 0xffff)) discShort discShort

instance Grouping Word64 where
  grouping = divide (\x -> ((fromIntegral (shiftR x 48) .&. 0xffff, fromIntegral (shiftR x 32) .&. 0xffff),
                            (fromIntegral (unsafeShiftR x 16) .&. 0xffff, fromIntegral x .&. 0xffff)))
                           (divide id discShort discShort) (divide id discShort discShort)

instance Grouping Word where
  grouping
    | (maxBound :: Word) == 4294967295
                = divide (\x -> (fromIntegral (unsafeShiftR x 16) .&. 0xffff, fromIntegral x .&. 0xffff)) discShort discShort
    | otherwise = divide (\x -> ((fromIntegral (shiftR x 48) .&. 0xffff, fromIntegral (shiftR x 32) .&. 0xffff),
                                 (fromIntegral (unsafeShiftR x 16) .&. 0xffff, fromIntegral x .&. 0xffff)))
                                (divide id discShort discShort) (divide id discShort discShort)

instance Grouping Int8 where
  grouping = contramap (\x -> fromIntegral x + 128) discShort

instance Grouping Int16 where
  grouping = contramap (\x -> fromIntegral x + 32768) discShort

instance Grouping Int32 where
  grouping = divide (\x -> let y = fromIntegral (x - minBound) in (unsafeShiftR y 16, y .&. 0xffff)) discShort discShort

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
  default grouping1 :: Deciding1 Grouping f => Disc a -> Disc (f a)
  grouping1 = deciding1 (Proxy :: Proxy Grouping) grouping

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

-- * Ordered Discrimination

-- | 'Ord' equipped with a compatible stable, ordered discriminator.
class Grouping a => Sorting a where
  sorting :: Disc a
  default sorting :: Deciding Sorting a => Disc a
  sorting = deciding (Proxy :: Proxy Sorting) sorting

instance Sorting Word8 where
  sorting = contramap fromIntegral (sdiscNat 256)

instance Sorting Word16 where
  sorting = contramap fromIntegral (sdiscNat 65536)

instance Sorting Word32 where
  sorting = divide (\x -> ((fromIntegral (shiftR x 48) .&. 0xffff, fromIntegral (shiftR x 32) .&. 0xffff),
                            (fromIntegral (unsafeShiftR x 16) .&. 0xffff, fromIntegral x .&. 0xffff))) go go where
    go = divide id (sdiscNat 65536) (sdiscNat 65536)

instance Sorting Word64 where
  sorting = divide (\x -> ((fromIntegral (shiftR x 48) .&. 0xffff, fromIntegral (shiftR x 32) .&. 0xffff),
                            (fromIntegral (unsafeShiftR x 16) .&. 0xffff, fromIntegral x .&. 0xffff))) go go where
    go = divide id (sdiscNat 65536) (sdiscNat 65536)

instance Sorting Word where
  sorting
    | (maxBound :: Word) == 4294967295
                = divide (\x -> (fromIntegral (unsafeShiftR x 16) .&. 0xffff, fromIntegral x .&. 0xffff)) (sdiscNat 65536) (sdiscNat 65536)
    | otherwise = divide (\x -> ((fromIntegral (shiftR x 48) .&. 0xffff, fromIntegral (shiftR x 32) .&. 0xffff),
                                 (fromIntegral (unsafeShiftR x 16) .&. 0xffff, fromIntegral x .&. 0xffff))) go go where
    go = divide id (sdiscNat 65536) (sdiscNat 65536)

instance Sorting Int8 where
  sorting = contramap (\x -> fromIntegral (x - minBound)) (sdiscNat 256)

instance Sorting Int16 where
  sorting = contramap (\x -> fromIntegral (x - minBound)) (sdiscNat 65536)

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
  default sorting1 :: Deciding1 Sorting f => Disc a -> Disc (f a)
  sorting1 = deciding1 (Proxy :: Proxy Sorting) sorting

instance (Sorting1 f, Sorting1 g) => Sorting1 (Compose f g) where
  sorting1 f = getCompose `contramap` sorting1 (sorting1 f)

instance Sorting1 []
instance Sorting1 Maybe
instance Sorting a => Sorting1 (Either a)

sdiscColl :: Foldable f => ([Int] -> Int -> [Int]) -> Disc k -> Disc (f k)
sdiscColl update r = Disc $ \xss -> let
    (kss, vs)           = unzip xss
    elemKeyNumAssocs    = groupNum (toList <$> kss)
    keyNumBlocks        = runDisc r elemKeyNumAssocs
    keyNumElemNumAssocs = groupNum keyNumBlocks
    sigs                = bdiscNat (length kss) update keyNumElemNumAssocs
    yss                 = zip sigs vs
  in filter (not . null) $ sorting1 (sdiscNat (length keyNumBlocks)) `runDisc` yss

groupNum :: [[k]] -> [(k,Int)]
groupNum kss = concat [ (,n) <$> ks | n <- [0..] | ks <- kss ]

sdiscBag :: Disc k -> Disc [k]
sdiscBag = sdiscColl updateBag

sdiscSet :: Disc k -> Disc [k]
sdiscSet = sdiscColl updateSet

discColl :: Foldable f => ([Int] -> Int -> [Int]) -> Disc k -> Disc (f k)
discColl update r = Disc $ \xss -> let
    (kss, vs)           = unzip xss
    elemKeyNumAssocs    = groupNum (toList <$> kss)
    keyNumBlocks        = runDisc r elemKeyNumAssocs
    keyNumElemNumAssocs = groupNum keyNumBlocks
    sigs                = bdiscNat (length kss) update keyNumElemNumAssocs
    yss                 = zip sigs vs
  in filter (not . null) $ grouping1 (discNat (length keyNumBlocks)) `runDisc` yss

discBag :: Disc k -> Disc [k]
discBag = discColl updateBag

discSet :: Disc k -> Disc [k]
discSet = discColl updateSet

updateBag :: [Int] -> Int -> [Int]
updateBag vs v = v : vs

updateSet :: [Int] -> Int -> [Int]
updateSet [] w = [w]
updateSet vs@(v:_) w
  | v == w    = vs
  | otherwise = w : vs
