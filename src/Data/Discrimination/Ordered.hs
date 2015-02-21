{-# LANGUAGE GADTs, TypeOperators, RankNTypes, DeriveDataTypeable, DefaultSignatures, FlexibleContexts #-}
{-# OPTIONS_GHC -rtsopts -threaded -fno-cse -fno-full-laziness #-}

module Data.Discrimination.Order
  ( -- * Order Discrimination
    Order(..)
  -- * Higher-order order Discrimination
  , Order1(..)
  -- * Generic programming utilities for order discrimination
  , GOrder
  , GOrder1
  ) where

import Control.Monad
import Data.Discrimination.Type
import Data.Discrimination.Disorder
import Data.Functor
import Data.Functor.Compose
import Data.Functor.Contravariant
import Data.Void
import Data.Word
import GHC.Generics
import Prelude hiding (read)

class Disorder a => Order a where
  order :: Disc a
  default order :: (Generic a, GOrder (Rep a)) => Disc a
  order = contramap from gorder

instance Order Word8 where
  order = contramap fromIntegral (orderNat 256)

instance Order Word16 where
  order = contramap fromIntegral (orderNat 65536)

-- TODO: more integral types

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

class GDisorder f => GOrder f where
  gorder :: Disc (f a)

instance GOrder V1 where
  gorder = Disc $ \vs -> [uncurry seq <$> vs]

instance GOrder U1 where
  gorder = Disc $ \vs -> [snd <$> vs]

instance (GOrder f, GOrder g) => GOrder (f :*: g) where
  gorder = Disc $ \xs -> gorder % [ (b, (c, d)) | (b :*: c,d) <- xs ] >>= (%) gorder

instance (GOrder f, GOrder g) => GOrder (f :+: g) where
  gorder = Disc $ \xs -> gorder % [ (k,v) | (L1 k, v) <- xs]
                      ++ gorder % [ (k,v) | (R1 k, v) <- xs]

instance Order a => GOrder (K1 i a) where
  gorder = contramap unK1 order

instance GOrder f => GOrder (M1 i c f) where
  gorder = contramap unM1 gorder

class Disorder1 f => Order1 f  where
  order1 :: Disc a -> Disc (f a)
  default order1 :: (Generic1 f, GOrder1 (Rep1 f)) => Disc a -> Disc (f a)
  order1 d = from1 `contramap` gorder1 d

instance (Order1 f, Order1 g) => Order1 (Compose f g) where
  order1 f = getCompose `contramap` order1 (order1 f)

instance Order1 []
instance Order1 Maybe
instance Order a => Order1 (Either a)

class GDisorder1 f => GOrder1 f where
  gorder1 :: Disc a -> Disc (f a)
  
instance GOrder1 V1 where
  gorder1 _ = Disc $ \vs -> [uncurry seq <$> vs]

instance GOrder1 U1 where
  gorder1 _ = Disc $ \vs -> [snd <$> vs]

instance (GOrder1 f, GOrder1 g) => GOrder1 (f :*: g) where
  gorder1 f = Disc $ \xs -> gorder1 f % [ (b, (c, d)) | (b :*: c,d) <- xs ] >>= (%) (gorder1 f)

instance (GOrder1 f, GOrder1 g) => GOrder1 (f :+: g) where
  gorder1 f = Disc $ \xs -> gorder1 f % [ (k,v) | (L1 k, v) <- xs]
                        ++ gorder1 f % [ (k,v) | (R1 k, v) <- xs]

instance Order a => GOrder1 (K1 i a) where
  gorder1 _ = contramap unK1 disc

instance GOrder1 Par1 where
  gorder1 = contramap unPar1

instance GOrder1 f => GOrder1 (M1 i c f) where
  gorder1 = contramap unM1 . gorder1

instance (Order1 f, GOrder1 g) => GOrder1 (f :.: g) where
  gorder1 = contramap unComp1 . order1 . gorder1

instance Order1 f => GOrder1 (Rec1 f) where
  gorder1 = contramap unRec1 . order1
