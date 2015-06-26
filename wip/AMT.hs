{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}
module AMT where

import Control.Applicative
import Control.Monad.ST
import Data.Bits
import Data.Bits.Extras
import Data.Foldable
import Data.Functor
import Data.Monoid
import Data.Primitive.Array
import Data.Traversable
import Data.Word
import Prelude hiding (lookup)

type Key = Word64
type Mask = Word16
type Offset = Int

pattern N = 16

data WordMap v
  = Nil
  | Tip  !Key v
  | Node !Key !Offset !Mask !(Array (WordMap v))

instance Show v => Show (WordMap v) where
  showsPrec _ Nil = showString "Nil"
  showsPrec d (Tip k v) = showParen (d > 10) $
     showString "Tip " . showsPrec 11 k . showChar ' ' . showsPrec 11 v
  showsPrec d (Node k o m as) = showParen (d > 10) $
     showString "Node " . showsPrec 11 k . showChar ' ' . showsPrec 11 o . showChar ' ' . showsPrec 11 m . showChar ' ' . showParen True 
     (showString "toArray " . showsPrec 11 n . showChar ' ' . showsPrec 11 (fromArray n as))
     where n = popCount m

-- Assumes n >= 1
mapArray :: Int -> (a -> b) -> Array a -> Array b
mapArray !n f !i = runST $ do
  o <- newArray n undefined
  let go 0 = return ()
      go k = do
        a <- indexArrayM i k 
        writeArray o k (f a)
        go (k - 1)
  go (n-1)
  unsafeFreezeArray o

fromArray :: Int -> Array a -> [a]
fromArray !n !arr = go 0 where
  go !k
    | k == n    = []
    | otherwise = indexArray arr k : go (k+1)

toArray :: Int -> [a] -> Array a
toArray n xs0 = runST $ do
  arr <- newArray n undefined
  let go k []     = return ()
      go k (x:xs) = writeArray arr k x >> go (k+1) xs
  go 0 xs0
  unsafeFreezeArray arr

instance Functor WordMap where
  fmap f = go where
    go Nil = Nil
    go (Tip k v) = Tip k (f v)
    go (Node k o m a) = Node k o m (mapArray (popCount m) go a)

instance Foldable WordMap where
  foldMap f = go where
    go Nil = mempty
    go (Tip _ v) = f v
    go (Node _ _ m a) = foldMap go (fromArray (popCount m) a)

instance Traversable WordMap where
  traverse f = go where
    go Nil = pure Nil
    go (Tip k v) = Tip k <$> f v
    go (Node k o m a) = Node k o m . toArray n <$> traverse go (fromArray n a) where n = popCount m 

-- Note: 'level 0' will return a negative shift.
level :: Key -> Int
level w = 60 - (nlz w .&. 0xfc)

maskBit :: Key -> Offset -> Int
maskBit k o = fromIntegral (unsafeShiftR k o .&. 0xf)

mask :: Key -> Offset -> Word16
mask k o = bit (maskBit k o)

offset :: Int -> Word16 -> Int
offset k w = popCount $ w .&. (bit k - 1)

insert :: Key -> v -> WordMap v -> WordMap v
insert k v Nil = Tip k v
insert k v ot@(Tip ok ov)
  | k == ok = t
  | o <- level (xor k ok) = Node k o (setBit (mask k o) (maskBit ok o))
       $ if k < ok then two t ot else two ot t
  where t = Tip k v
insert k v on@(Node ok n m as)
  | o <- level (xor k ok)
  = if
    | o > n -> Node k o (setBit (mask k o) (maskBit k o)) $ if k < ok then two (Tip k v) on else two on (Tip k v)
    | d <- maskBit k n -> if 
      | testBit m (maskBit k n) -> Node ok n m            (updateArray d (popCount m) (insert k v) as)
      | otherwise               -> Node ok n (setBit m d) (insertArray (offset d m) (popCount m) (Tip k v) as)

lookup :: Key -> WordMap v -> Maybe v
lookup _ Nil = Nothing
lookup k (Tip ok ov)
  | k == ok   = Just ov
  | otherwise = Nothing
lookup k (Node ok o m a) 
  | testBit m d = lookup k $ indexArray a (offset d m)
  | otherwise = Nothing
  where d = maskBit k o
    
updateArray :: Int -> Int -> (a -> a) -> Array a -> Array a
updateArray k n f i = runST $ do
  o <- newArray n undefined
  copyArray o 0 i 0 n
  a <- indexArrayM i k 
  writeArray o k (f a)
  unsafeFreezeArray o

insertArray :: Int -> Int -> a -> Array a -> Array a
insertArray k n a i = runST $ do
  o <- newArray (n + 1) undefined
  copyArray  o 0 i 0 k 
  writeArray o k a
  copyArray  o (k+1) i k (n-k)
  unsafeFreezeArray o

two :: a -> a -> Array a
two a b = runST $ do
  arr <- newArray 2 a
  writeArray arr 1 b
  unsafeFreezeArray arr

-- so we need to be able to quickly check if we differ on a higher nybble than the one
-- the word32s are the least and greatest keys possible in this node, not present

singleton :: Key -> v -> WordMap v
singleton k v = Tip k v
