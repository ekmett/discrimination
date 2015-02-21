{-# LANGUAGE GADTs, TypeOperators, RankNTypes, DeriveDataTypeable, DefaultSignatures, FlexibleContexts #-}
-- {-# LANGUAGE StandaloneDeriving, DeriveGeneric #-}
{-# OPTIONS_GHC -rtsopts -threaded -fno-cse -fno-full-laziness #-}

module Data.Discrimination.Class
  ( -- * Unordered Discrimination
    Disorderable(..)
  , Disorder(..)
  , Disorder1(..)
    -- * Ordered Discrimination
  , Orderable(..)
  , Order(..)
  , Order1(..)
  ) where

import Data.Bits
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

class Decidable f => Disorderable f where
  -- | Discriminate integers 65536 values at a time without revealing ordering
  discShort :: f Int

-- | 'Eq' equipped with a compatible stable unordered discriminator.
class Disorder a where
  disorder :: Disorderable f => f a
  default disorder :: (Deciding Disorder a, Disorderable f) => f a
  disorder = deciding (Proxy :: Proxy Disorder) disorder

instance Disorder Void where
  disorder = lose id

instance Disorder Word8 where
  disorder = contramap fromIntegral discShort 

instance Disorder Word16 where
  disorder = contramap fromIntegral discShort 

instance Disorder Word32 where
  disorder = divide (\x -> let y = fromIntegral x in (unsafeShiftR y 16 .&. 65535, y .&. 65535)) discShort discShort

instance Disorder Int8 where
  disorder = contramap ((+128) . fromIntegral) discShort

instance Disorder Int16 where
  disorder = contramap ((+32768) . fromIntegral) discShort

instance Disorder Int32 where
  disorder = divide (\x -> let y = fromIntegral x - 2147483648 in (unsafeShiftR y 16 .&. 65535, y .&. 65535)) discShort discShort

instance Disorder Bool
instance (Disorder a, Disorder b) => Disorder (a, b)
instance (Disorder a, Disorder b, Disorder c) => Disorder (a, b, c)
instance (Disorder a, Disorder b, Disorder c, Disorder d) => Disorder (a, b, c, d)
instance Disorder a => Disorder [a]
instance Disorder a => Disorder (Maybe a)
instance (Disorder a, Disorder b) => Disorder (Either a b)
instance (Disorder1 f, Disorder1 g, Disorder a) => Disorder (Compose f g a) where
  disorder = getCompose `contramap` disorder1 (disorder1 disorder)

class Disorder1 f where
  disorder1 :: Disorderable g => g a -> g (f a)
  default disorder1 :: (Disorderable g, Deciding1 Disorder f) => g a -> g (f a)
  disorder1 = deciding1 (Proxy :: Proxy Disorder) disorder

instance Disorder1 []
instance Disorder1 Maybe
instance Disorder a => Disorder1 (Either a)
instance Disorder a => Disorder1 ((,) a)
instance (Disorder a, Disorder b) => Disorder1 ((,,) a b)
instance (Disorder a, Disorder b, Disorder c) => Disorder1 ((,,,) a b c)
instance (Disorder1 f, Disorder1 g) => Disorder1 (Compose f g) where
  disorder1 f = getCompose `contramap` disorder1 (disorder1 f)

-- * Ordered Discrimination

class Decidable f => Orderable f where
  -- | Discriminate integers in order
  sdiscNat :: Int -> f Int

  -- | Reverse the ordering
  desc :: f a -> f a

-- | 'Ord' equipped with a compatible stable, ordered discriminator.
class Disorder a => Order a where
  order :: Orderable f => f a
  default order :: (Orderable f, Deciding Order a) => f a
  order = deciding (Proxy :: Proxy Order) order

instance Order Word8 where
  order = contramap fromIntegral (sdiscNat 256)

instance Order Word16 where
  order = contramap fromIntegral (sdiscNat 65536)

instance Order Void
instance Order Bool
instance Order a => Order [a]
instance Order a => Order (Maybe a)
instance (Order a, Order b) => Order (Either a b)
instance (Order a, Order b) => Order (a, b)
instance (Order a, Order b, Order c) => Order (a, b, c)
instance (Order a, Order b, Order c, Order d) => Order (a, b, c, d)
instance (Order1 f, Order1 g, Order a) => Order (Compose f g a) where
  order = getCompose `contramap` order1 (order1 order)

class Disorder1 f => Order1 f  where
  order1 :: Orderable g => g a -> g (f a)
  default order1 :: (Orderable g, Deciding1 Order f) => g a -> g (f a)
  order1 = deciding1 (Proxy :: Proxy Order) order

instance (Order1 f, Order1 g) => Order1 (Compose f g) where
  order1 f = getCompose `contramap` order1 (order1 f)

instance Order1 []
instance Order1 Maybe
instance Order a => Order1 (Either a)
