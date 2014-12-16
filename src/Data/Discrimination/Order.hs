module Data.Discrimination.Order where

import Data.Discrimination.Discriminating
import Data.Discrimination.Type
import Data.Functor.Contravariant
import Data.Functor.Contravariant.Divisible
import Data.Reflection

-- This is a legal contravariant functor
-- but it is a legal Decidable functor only when viewed abstractly through the quotient of 'disc'.
data Order a where
  Natural :: (a -> Int) -> Int -> Order a
  Divide  :: (a -> (b, c)) -> Order b -> Order c -> Order a
  Conquer :: Order a
  Choose  :: (a -> Either b c) -> Order b -> Order c -> Order a
  Lose    :: (a -> Void) -> Order a
  Bag     :: (a -> [b]) -> Order b -> Order a
  Set     :: (a -> [b]) -> Order b -> Order a
  Desc    :: Order a -> Order a

instance Discriminating Order where
  nat = Natural id
  bag = Bag id
  set = Set id

instance Contravariant Order where
  contramap f (Natural k n) = Natural (k.f) n
  contramap f (Divide k l r) = Divide (k.f) l r
  contramap _ Conquer = Conquer
  contramap f (Lose k) = Lose (k.f)
  contramap f (Choose k l r) = Choose (k.f) l r
  contramap f (Bag k r) = Bag (k.f) r
  contramap f (Set k r) = Bag (k.f) r
  contramap f (Desc m) = Desc (contramap f m)

instance Divisible Order where
  divide = Divide
  conquer = Conquer

instance Decidable Order where
  choose = Choose
  lose = Lose

instance Monoid (Order a) where
  mempty = conquer
  mappend = divide (id &&& id)

-- instance Ordered a => Default (Order a) where
--   def = sdisc

sdisc :: Order a -> Disc a
sdisc Conquer        = conquer
sdisc (Natural k n)  = k `contramap` sdiscNat n
sdisc (Divide k l r) = divide k l r
sdisc (Lose k)       = lose k
sdisc (Choose k l r) = choose k l r
-- sdisc (Bag k r)      = sdiscColl insertBag a
-- sdisc (Set k r)      = 
sdisc (Desc r)       = descending (sdisc r)

compareWith :: Order a -> a -> a -> Ordering
compareWith Conquer _ _ = EQ
compareWith (Natural k n) a b = compare (k a) (k b)
compareWith (Divide k l r) a b = case k a of
  (a',a'') -> case k b of 
    (b',b'') -> compareWith l a' b' <> compareWith r a'' b''
compareWith (Lose k) a b = k a `seq` k b `seq` undefined
compareWith (Choose k l r) a b = case k a of
  Left c -> case k b of 
    Left d  -> compareWith l c d
    Right _ -> LT
  Right c -> case k b of
    Left _ -> GT
    Right d -> compareWith r c d
-- compareWith (Bag k r) a b
-- compareWith (Set k r) a b
compareWith (Desc r) a b = compareWith r b a
