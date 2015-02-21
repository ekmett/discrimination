{-# LANGUAGE GADTs, TypeOperators, RankNTypes, DeriveDataTypeable, DefaultSignatures, FlexibleContexts #-}
{-# OPTIONS_GHC -rtsopts -threaded -fno-cse -fno-full-laziness #-}

module Data.Discrimination.Disordered
  ( -- * Disordered Discrimination
    Disordered(..)
    -- * Higher-order Disordered Discrimination
  , Disordered1(..)
    -- * Generic Disordered Discrimination
  , GDisordered
  , GDisordered1
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

class Disordered a where
  disorder :: Disc a
  default disorder :: (Generic a, GDisordered (Rep a)) => Disc a
  disorder = contramap from gdisorder

instance Disordered Void where
  disorder = Disc $ \vs -> [uncurry seq <$> vs]

instance Disordered Word8 where
  disorder = contramap fromIntegral disorderShort 

instance Disordered Word16 where
  disorder = contramap fromIntegral disorderShort 

instance Disordered Word32 where
  disorder = divide (\x -> let y = fromIntegral x in (unsafeShiftR y 16 .&. 65535, y .&. 65535)) disorderShort disorderShort

instance Disordered Int8 where
  disorder = contramap (subtract 128 . fromIntegral) disorderShort

instance Disordered Int16 where
  disorder = contramap (subtract 32768 . fromIntegral) disorderShort

instance Disordered Int32 where
  disorder = divide (\x -> let y = fromIntegral x - 2147483648 in (unsafeShiftR y 16 .&. 65535, y .&. 65535)) disorderShort disorderShort

instance Disordered Bool
instance (Disordered a, Disordered b) => Disordered (a, b)
instance (Disordered a, Disordered b, Disordered c) => Disordered (a, b, c)
instance (Disordered a, Disordered b, Disordered c, Disordered d) => Disordered (a, b, c, d)
instance Disordered a => Disordered [a]
instance Disordered a => Disordered (Maybe a)
instance (Disordered a, Disordered b) => Disordered (Either a b)
instance (Disordered1 f, Disordered1 g, Disordered a) => Disordered (Compose f g a) where
  disorder = getCompose `contramap` disorder1 (disorder1 disorder)

class GDisordered f where
  gdisorder :: Disc (f a)

instance GDisordered V1 where
  gdisorder = Disc $ \vs -> [uncurry seq <$> vs]

instance GDisordered U1 where
  gdisorder = Disc $ \vs -> [snd <$> vs]

instance (GDisordered f, GDisordered g) => GDisordered (f :*: g) where
  gdisorder = Disc $ \xs -> gdisorder % [ (b, (c, d)) | (b :*: c,d) <- xs ] >>= (%) gdisorder

instance (GDisordered f, GDisordered g) => GDisordered (f :+: g) where
  gdisorder = Disc $ \xs -> gdisorder % [ (k,v) | (L1 k, v) <- xs]
                     ++ gdisorder % [ (k,v) | (R1 k, v) <- xs]

instance Disordered a => GDisordered (K1 i a) where
  gdisorder = contramap unK1 disorder

instance GDisordered f => GDisordered (M1 i c f) where
  gdisorder = contramap unM1 gdisorder

class Disordered1 f where
  disorder1 :: Disc a -> Disc (f a)
  default disorder1 :: (Generic1 f, GDisordered1 (Rep1 f)) => Disc a -> Disc (f a)
  disorder1 d = from1 `contramap` gdisorder1 d

instance Disordered1 []
instance Disordered1 Maybe
instance Disordered a => Disordered1 (Either a)
instance Disordered a => Disordered1 ((,) a)
instance (Disordered a, Disordered b) => Disordered1 ((,,) a b)
instance (Disordered a, Disordered b, Disordered c) => Disordered1 ((,,,) a b c)
instance (Disordered1 f, Disordered1 g) => Disordered1 (Compose f g) where
  disorder1 f = getCompose `contramap` disorder1 (disorder1 f)
  
class GDisordered1 f where
  gdisorder1 :: Disc a -> Disc (f a)

instance GDisordered1 V1 where
  gdisorder1 _ = Disc $ \vs -> [uncurry seq <$> vs]

instance GDisordered1 U1 where
  gdisorder1 _ = Disc $ \vs -> [snd <$> vs]

instance (GDisordered1 f, GDisordered1 g) => GDisordered1 (f :*: g) where
  gdisorder1 f = Disc $ \xs -> gdisorder1 f % [ (b, (c, d)) | (b :*: c,d) <- xs ] >>= (%) (gdisorder1 f)

instance (GDisordered1 f, GDisordered1 g) => GDisordered1 (f :+: g) where
  gdisorder1 f = Disc $ \xs -> gdisorder1 f % [ (k,v) | (L1 k, v) <- xs]
                        ++ gdisorder1 f % [ (k,v) | (R1 k, v) <- xs]

instance Disordered a => GDisordered1 (K1 i a) where
  gdisorder1 _ = contramap unK1 disorder 

instance GDisordered1 Par1 where
  gdisorder1 = contramap unPar1

instance GDisordered1 f => GDisordered1 (M1 i c f) where
  gdisorder1 = contramap unM1 . gdisorder1

instance (Disordered1 f, GDisordered1 g) => GDisordered1 (f :.: g) where
  gdisorder1 = contramap unComp1 . disorder1 . gdisorder1

instance Disordered1 f => GDisordered1 (Rec1 f) where
  gdisorder1 = contramap unRec1 . disorder1
