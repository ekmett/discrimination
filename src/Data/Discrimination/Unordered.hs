{-# LANGUAGE GADTs, TypeOperators, RankNTypes, DeriveDataTypeable, DefaultSignatures, FlexibleContexts #-}
{-# OPTIONS_GHC -rtsopts -threaded -fno-cse -fno-full-laziness #-}

module Data.Discrimination.Unordered
  ( -- * Unordered Discrimination
    Unordered(..)
    -- * Higher-order Unordered Discrimination
  , Unordered1(..)
    -- * Generic Unordered Discrimination
  , GUnordered
  , GUnordered1
  ) where

import Control.Monad
import Data.Bits
import Data.Discrimination.Type
import Data.Functor
import Data.Functor.Compose
import Data.Functor.Contravariant
import Data.Functor.Contravariant.Divisible
import Data.Int
import Data.Void
import Data.Word
import GHC.Generics
import Prelude hiding (read)

class Unordered a where
  disc :: Disc a
  default disc :: (Generic a, GUnordered (Rep a)) => Disc a
  disc = contramap from gdisc

instance Unordered Void where
  disc = Disc $ \vs -> [uncurry seq <$> vs]

instance Unordered Word8 where
  disc = contramap fromIntegral discShort 

instance Unordered Word16 where
  disc = contramap fromIntegral discShort 

instance Unordered Word32 where
  disc = divide (\x -> let y = fromIntegral x in (unsafeShiftR y 16 .&. 65535, y .&. 65535)) discShort discShort

instance Unordered Int8 where
  disc = contramap (subtract 128 . fromIntegral) discShort

instance Unordered Int16 where
  disc = contramap (subtract 32768 . fromIntegral) discShort

instance Unordered Int32 where
  disc = divide (\x -> let y = fromIntegral x - 2147483648 in (unsafeShiftR y 16 .&. 65535, y .&. 65535)) discShort discShort

instance Unordered Bool
instance (Unordered a, Unordered b) => Unordered (a, b)
instance (Unordered a, Unordered b, Unordered c) => Unordered (a, b, c)
instance (Unordered a, Unordered b, Unordered c, Unordered d) => Unordered (a, b, c, d)
instance Unordered a => Unordered [a]
instance Unordered a => Unordered (Maybe a)
instance (Unordered a, Unordered b) => Unordered (Either a b)
instance (Unordered1 f, Unordered1 g, Unordered a) => Unordered (Compose f g a) where
  disc = getCompose `contramap` disc1 (disc1 disc)

class GUnordered f where
  gdisc :: Disc (f a)

instance GUnordered V1 where
  gdisc = Disc $ \vs -> [uncurry seq <$> vs]

instance GUnordered U1 where
  gdisc = Disc $ \vs -> [snd <$> vs]

instance (GUnordered f, GUnordered g) => GUnordered (f :*: g) where
  gdisc = Disc $ \xs -> gdisc % [ (b, (c, d)) | (b :*: c,d) <- xs ] >>= (%) gdisc

instance (GUnordered f, GUnordered g) => GUnordered (f :+: g) where
  gdisc = Disc $ \xs -> gdisc % [ (k,v) | (L1 k, v) <- xs]
                     ++ gdisc % [ (k,v) | (R1 k, v) <- xs]

instance Unordered a => GUnordered (K1 i a) where
  gdisc = contramap unK1 disc

instance GUnordered f => GUnordered (M1 i c f) where
  gdisc = contramap unM1 gdisc

class Unordered1 f where
  disc1 :: Disc a -> Disc (f a)
  default disc1 :: (Generic1 f, GUnordered1 (Rep1 f)) => Disc a -> Disc (f a)
  disc1 d = from1 `contramap` gdisc1 d

instance Unordered1 []
instance Unordered1 Maybe
instance Unordered a => Unordered1 (Either a)
instance Unordered a => Unordered1 ((,) a)
instance (Unordered a, Unordered b) => Unordered1 ((,,) a b)
instance (Unordered a, Unordered b, Unordered c) => Unordered1 ((,,,) a b c)
instance (Unordered1 f, Unordered1 g) => Unordered1 (Compose f g) where
  disc1 f = getCompose `contramap` disc1 (disc1 f)
  
class GUnordered1 f where
  gdisc1 :: Disc a -> Disc (f a)

instance GUnordered1 V1 where
  gdisc1 _ = Disc $ \vs -> [uncurry seq <$> vs]

instance GUnordered1 U1 where
  gdisc1 _ = Disc $ \vs -> [snd <$> vs]

instance (GUnordered1 f, GUnordered1 g) => GUnordered1 (f :*: g) where
  gdisc1 f = Disc $ \xs -> gdisc1 f % [ (b, (c, d)) | (b :*: c,d) <- xs ] >>= (%) (gdisc1 f)

instance (GUnordered1 f, GUnordered1 g) => GUnordered1 (f :+: g) where
  gdisc1 f = Disc $ \xs -> gdisc1 f % [ (k,v) | (L1 k, v) <- xs]
                        ++ gdisc1 f % [ (k,v) | (R1 k, v) <- xs]

instance Unordered a => GUnordered1 (K1 i a) where
  gdisc1 _ = contramap unK1 disc 

instance GUnordered1 Par1 where
  gdisc1 = contramap unPar1

instance GUnordered1 f => GUnordered1 (M1 i c f) where
  gdisc1 = contramap unM1 . gdisc1

instance (Unordered1 f, GUnordered1 g) => GUnordered1 (f :.: g) where
  gdisc1 = contramap unComp1 . disc1 . gdisc1

instance Unordered1 f => GUnordered1 (Rec1 f) where
  gdisc1 = contramap unRec1 . disc1
