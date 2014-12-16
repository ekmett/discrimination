{-# LANGUAGE GADTs, TypeOperators, RankNTypes, DeriveDataTypeable, DefaultSignatures, FlexibleContexts #-}
{-# OPTIONS_GHC -rtsopts -threaded -fno-cse -fno-full-laziness #-}

module Data.Discrimination.Ordered
  ( -- * Ordered Discrimination
    Ordered(..)
  -- * Higher-order ordered Discrimination
  , Ordered1(..)
  -- * Generic programming utilities for ordered discrimination
  , GOrdered
  , GOrdered1
  ) where

import Control.Monad
import Data.Discrimination.Type
import Data.Discrimination.Unordered
import Data.Functor
import Data.Functor.Compose
import Data.Functor.Contravariant
import Data.Void
import Data.Word
import GHC.Generics
import Prelude hiding (read)

class Unordered a => Ordered a where
  sdisc :: Disc a
  default sdisc :: (Generic a, GOrdered (Rep a)) => Disc a
  sdisc = contramap from gsdisc

instance Ordered Word8 where
  sdisc = contramap fromIntegral (sdiscNat 256)

instance Ordered Word16 where
  sdisc = contramap fromIntegral (sdiscNat 65536)

-- TODO: more integral types

instance Ordered Void
instance Ordered Bool
instance Ordered a => Ordered [a]
instance Ordered a => Ordered (Maybe a)
instance (Ordered a, Ordered b) => Ordered (Either a b)
instance (Ordered a, Ordered b) => Ordered (a, b)
instance (Ordered a, Ordered b, Ordered c) => Ordered (a, b, c)
instance (Ordered a, Ordered b, Ordered c, Ordered d) => Ordered (a, b, c, d)
instance (Ordered1 f, Ordered1 g, Ordered a) => Ordered (Compose f g a) where
  sdisc = getCompose `contramap` sdisc1 (sdisc1 sdisc)

class GUnordered f => GOrdered f where
  gsdisc :: Disc (f a)

instance GOrdered V1 where
  gsdisc = Disc $ \vs -> [uncurry seq <$> vs]

instance GOrdered U1 where
  gsdisc = Disc $ \vs -> [snd <$> vs]

instance (GOrdered f, GOrdered g) => GOrdered (f :*: g) where
  gsdisc = Disc $ \xs -> gsdisc % [ (b, (c, d)) | (b :*: c,d) <- xs ] >>= (%) gsdisc

instance (GOrdered f, GOrdered g) => GOrdered (f :+: g) where
  gsdisc = Disc $ \xs -> gsdisc % [ (k,v) | (L1 k, v) <- xs]
                      ++ gsdisc % [ (k,v) | (R1 k, v) <- xs]

instance Ordered a => GOrdered (K1 i a) where
  gsdisc = contramap unK1 sdisc

instance GOrdered f => GOrdered (M1 i c f) where
  gsdisc = contramap unM1 gsdisc

class Unordered1 f => Ordered1 f  where
  sdisc1 :: Disc a -> Disc (f a)
  default sdisc1 :: (Generic1 f, GOrdered1 (Rep1 f)) => Disc a -> Disc (f a)
  sdisc1 d = from1 `contramap` gsdisc1 d

instance (Ordered1 f, Ordered1 g) => Ordered1 (Compose f g) where
  sdisc1 f = getCompose `contramap` sdisc1 (sdisc1 f)

instance Ordered1 []
instance Ordered1 Maybe
instance Ordered a => Ordered1 (Either a)

class GUnordered1 f => GOrdered1 f where
  gsdisc1 :: Disc a -> Disc (f a)
  
instance GOrdered1 V1 where
  gsdisc1 _ = Disc $ \vs -> [uncurry seq <$> vs]

instance GOrdered1 U1 where
  gsdisc1 _ = Disc $ \vs -> [snd <$> vs]

instance (GOrdered1 f, GOrdered1 g) => GOrdered1 (f :*: g) where
  gsdisc1 f = Disc $ \xs -> gsdisc1 f % [ (b, (c, d)) | (b :*: c,d) <- xs ] >>= (%) (gsdisc1 f)

instance (GOrdered1 f, GOrdered1 g) => GOrdered1 (f :+: g) where
  gsdisc1 f = Disc $ \xs -> gsdisc1 f % [ (k,v) | (L1 k, v) <- xs]
                        ++ gsdisc1 f % [ (k,v) | (R1 k, v) <- xs]

instance Ordered a => GOrdered1 (K1 i a) where
  gsdisc1 _ = contramap unK1 disc

instance GOrdered1 Par1 where
  gsdisc1 = contramap unPar1

instance GOrdered1 f => GOrdered1 (M1 i c f) where
  gsdisc1 = contramap unM1 . gsdisc1

instance (Ordered1 f, GOrdered1 g) => GOrdered1 (f :.: g) where
  gsdisc1 = contramap unComp1 . sdisc1 . gsdisc1

instance Ordered1 f => GOrdered1 (Rec1 f) where
  gsdisc1 = contramap unRec1 . sdisc1
