{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE FlexibleContexts #-}
module Data.Discrimination.Generic
  ( Deciding(..)
  , Deciding1(..)
  ) where

import Data.Functor.Contravariant
import Data.Functor.Contravariant.Divisible
import GHC.Generics

-- | Machinery for deconstructing an arbitrary 'Generic' instance using a 'Decidable' 'Contravariant' functor.
class (Generic a, GDeciding q (Rep a)) => Deciding q a where
  deciding :: Decidable f => p q -> (forall b. q b => f b) -> f a

instance (Generic a, GDeciding q (Rep a)) => Deciding q a  where
  deciding p q = contramap from $ gdeciding p q

-- | Machinery for deconstructing an arbitrary 'Generic1' instance using a 'Decidable' 'Contravariant' functor.
class (Generic1 t, GDeciding1 q (Rep1 t)) => Deciding1 q t where
  deciding1 :: Decidable f => p q -> (forall b. q b => f b) -> f a -> f (t a)

instance (Generic1 t, GDeciding1 q (Rep1 t)) => Deciding1 q t where
  deciding1 p q r = contramap from1 $ gdeciding1 p q r

class GDeciding q t where
  gdeciding :: Decidable f => p q -> (forall b. q b => f b) -> f (t a)

instance GDeciding q U1 where
  gdeciding _ _ = conquer

instance GDeciding q V1 where
  gdeciding _ _ = lose (\case {})

instance (GDeciding q f, GDeciding q g) => GDeciding q (f :*: g) where
  gdeciding p q = divide (\(a :*: b) -> (a, b)) (gdeciding p q) (gdeciding p q)

instance (GDeciding q f, GDeciding q g) => GDeciding q (f :+: g) where
  gdeciding p q = choose (\case L1 a -> Left a; R1 a -> Right a) (gdeciding p q) (gdeciding p q)

instance q p => GDeciding q (K1 i p) where
  gdeciding _ q = contramap unK1 q

instance GDeciding q f => GDeciding q (M1 i c f) where
  gdeciding p q = contramap unM1 (gdeciding p q)

class GDeciding1 q t where
  gdeciding1 :: Decidable f => p q -> (forall b. q b => f b) -> f a -> f (t a)

instance GDeciding1 q U1 where
  gdeciding1 _ _ _ = conquer

instance GDeciding1 q V1 where
  gdeciding1 _ _ _ = lose (\case {})

instance (GDeciding1 q f, GDeciding1 q g) => GDeciding1 q (f :*: g) where
  gdeciding1 p q r = divide (\(a :*: b) -> (a, b)) (gdeciding1 p q r) (gdeciding1 p q r)

instance (GDeciding1 q f, GDeciding1 q g) => GDeciding1 q (f :+: g) where
  gdeciding1 p q r = choose (\case L1 a -> Left a; R1 a -> Right a) (gdeciding1 p q r) (gdeciding1 p q r)

instance q p => GDeciding1 q (K1 i p) where
  gdeciding1 _ q _ = contramap unK1 q

instance GDeciding1 q f => GDeciding1 q (M1 i c f) where
  gdeciding1 p q r = contramap unM1 (gdeciding1 p q r)

instance GDeciding1 q Par1 where
  gdeciding1 _ _ r = contramap unPar1 r

-- instance GDeciding1 q f => GDeciding1 q (Rec1 f) where gdeciding1 p q r = contramap unRec1 (gdeciding1 p q r)

instance Deciding1 q f => GDeciding1 q (Rec1 f) where 
  gdeciding1 p q r = contramap unRec1 (deciding1 p q r)
