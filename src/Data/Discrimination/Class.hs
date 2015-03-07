{-# LANGUAGE GADTs, TypeOperators, RankNTypes, DeriveDataTypeable, DefaultSignatures, FlexibleContexts #-}
{-# OPTIONS_GHC -fno-cse -fno-full-laziness #-}

module Data.Discrimination.Class
  ( -- * Unordered Discrimination
    Grouped(..)
  , Grouping(..)
  , Grouping1(..)
    -- * Ordered Discrimination
  , Sorted(..)
  , Sorting(..)
  , Sorting1(..)
  ) where

import Data.Bits
import Data.Complex
import Data.Ratio
import Data.Discrimination.Generic
import Data.Functor.Compose
import Data.Functor.Contravariant
import Data.Functor.Contravariant.Divisible
import Data.Int
import Data.Proxy
import Data.Void
import Data.Word
import Prelude hiding (read)

-- * Unordered Discrimination (for partitioning)

class Decidable f => Grouped f where
  -- | Discriminate integers 65536 values at a time without revealing ordering
  discShort :: f Int

-- | 'Eq' equipped with a compatible stable unordered discriminator.
class Grouping a where
  grouping :: Grouped f => f a
  default grouping :: (Grouped f, Deciding Grouping a) => f a
  grouping = deciding (Proxy :: Proxy Grouping) grouping

instance Grouping Void where
  grouping = lose id

instance Grouping Word8 where
  grouping = contramap fromIntegral discShort 

instance Grouping Word16 where
  grouping = contramap fromIntegral discShort 

instance Grouping Word32 where
  grouping = divide (\x -> (fromIntegral (unsafeShiftR x 16), fromIntegral x)) discShort discShort

instance Grouping Word64 where
  grouping = divide (\x -> (fromIntegral (unsafeShiftR x 32) :: Word32, fromIntegral x :: Word32)) grouping grouping

instance Grouping Word where
  grouping = divide (\x -> (fromIntegral (unsafeShiftR x 32) :: Word32, fromIntegral x :: Word32)) grouping grouping

instance Grouping Int8 where
  grouping = contramap (\x -> fromIntegral x + 128) discShort

instance Grouping Int16 where
  grouping = contramap (\x -> fromIntegral x + 32768) discShort

instance Grouping Int32 where
  grouping = divide (\x -> let y = fromIntegral (x - minBound) in (unsafeShiftR y 16, y)) discShort discShort

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
  grouping1 :: Grouped g => g a -> g (f a)
  default grouping1 :: (Grouped g, Deciding1 Grouping f) => g a -> g (f a)
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

class Decidable f => Sorted f where
  -- | Discriminate integers in order
  sdiscNat :: Int -> f Int

  -- | Reverse the ordering
  desc :: f a -> f a

-- | 'Ord' equipped with a compatible stable, ordered discriminator.
class Grouping a => Sorting a where
  sorting :: Sorted f => f a
  default sorting :: (Sorted f, Deciding Sorting a) => f a
  sorting = deciding (Proxy :: Proxy Sorting) sorting

instance Sorting Word8 where
  sorting = contramap fromIntegral (sdiscNat 256)

instance Sorting Word16 where
  sorting = contramap fromIntegral (sdiscNat 65536)

instance Sorting Word32 where
  sorting = divide (\x -> (fromIntegral (unsafeShiftR x 16), fromIntegral x)) (sdiscNat 65536) (sdiscNat 65536)

instance Sorting Word64 where
  sorting = divide (\x -> (fromIntegral (unsafeShiftR x 32) :: Word32, fromIntegral x :: Word32)) sorting sorting

instance Sorting Word where
  sorting = divide (\x -> (fromIntegral (unsafeShiftR x 32) :: Word32, fromIntegral x :: Word32)) sorting sorting

instance Sorting Int8 where
  sorting = contramap ((+128) . fromIntegral) (sdiscNat 256)

instance Sorting Int16 where
  sorting = contramap ((+32768) . fromIntegral) (sdiscNat 65536)

instance Sorting Int32 where
  sorting = divide (\x -> let y = fromIntegral (x - minBound) in (unsafeShiftR y 16, y)) (sdiscNat 65536) (sdiscNat 65536)

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
  sorting1 :: Sorted g => g a -> g (f a)
  default sorting1 :: (Sorted g, Deciding1 Sorting f) => g a -> g (f a)
  sorting1 = deciding1 (Proxy :: Proxy Sorting) sorting

instance (Sorting1 f, Sorting1 g) => Sorting1 (Compose f g) where
  sorting1 f = getCompose `contramap` sorting1 (sorting1 f)

instance Sorting1 []
instance Sorting1 Maybe
instance Sorting a => Sorting1 (Either a)
