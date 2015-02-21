{-# LANGUAGE CPP #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
module Data.Discrimination.Table
  ( Table(Table, runTable)
  , table
  , filterMap
  , reverse
  , null
  , length
  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Fix
import Control.Monad.Zip
import Data.Foldable as Foldable
import Data.Function (on)
import Data.Monoid
import Data.Traversable
import GHC.Exts as Exts
import Prelude hiding 
  ( reverse
#if __GLASGOW_HASKELL__ < 710
  , null, length
#endif
  )

data Table a = Table
  { count :: {-# UNPACK #-} !Int -- an opaque recognizer
  , runTable :: forall r. Monoid r => (a -> r) -> r
  }

instance Show a => Show (Table a) where
  showsPrec d t = showParen (d > 10) $ showString "fromList " . showsPrec 11 (Exts.toList t)

instance Eq a => Eq (Table a) where
  a@(Table n _) == b@(Table m _) = n == m && Exts.toList a == Exts.toList b

instance Ord a => Ord (Table a) where
  compare = on compare Exts.toList

reverse :: Table a -> Table a
reverse (Table n m) = Table n $ \k -> getDual $ m (Dual . k)

instance Functor Table where
  fmap f (Table i m) = Table i $ \k -> m (k.f)

instance Foldable Table where
  foldMap f (Table _ m) = m f
  foldr f z (Table _ m) = m (Endo . f) `appEndo` z
#if __GLASGOW_HASKELL__ >= 710
  null t = count t == 0
  length = count
#endif

#if __GLASGOW_HASKELL__ < 710
null :: Table a -> Bool
null t = count t == 0
{-# INLINE null #-}

length :: Table a -> Int
length = count
#endif

instance Monoid (Table a) where
  mempty = Table 0 $ \_ -> mempty
  mappend (Table i m) (Table j n) = Table (i + j) $ \k -> m k `mappend` n k

newtype Ap f a = Ap { runAp :: f a }

instance (Applicative f, Monoid a) => Monoid (Ap f a) where
  mempty = Ap (pure mempty)
  mappend (Ap m) (Ap n) = Ap (liftA2 mappend m n)

instance Traversable Table where
  -- this reassembles the result with sharing
  traverse f = runAp . foldMap (Ap . fmap pure . f)

instance Applicative Table where
  pure a = Table 1 $ \k -> k a
  Table n as <*> Table m bs = Table (n * m) $ \k -> as $ \f -> bs (k . f)
  Table n as <*  Table m _  = Table (n * m) $ \k -> as (rep m . k)
  Table n _   *> Table m bs = Table (n * m) $ rep n . bs

instance IsList (Table a) where
  type Item (Table a) = a
  fromList  = foldMap pure
  toList    = Foldable.toList
  fromListN n xs = Table n (`foldMap` xs)

-- peasant multiplication
rep :: Monoid m => Int -> m -> m
rep y0 x0
  | y0 <= 0   = mempty
  | otherwise = f x0 y0
  where
    f x y 
      | even y = f (mappend x x) (quot y 2)
      | y == 1 = x
      | otherwise = g (mappend x x) (quot (y - 1) 2) x
    g x y z 
      | even y = g (mappend x x) (quot y 2) z
      | y == 1 = mappend x z
      | otherwise = g (mappend x x) (quot (y - 1) 2) (mappend x z)

table :: (forall m. Monoid m => (a -> m) -> m) -> Table a
table k = Table (getSum $ k $ \_ -> Sum 1) k
{-# INLINE table #-}

instance Monad Table where
  return a = Table 1 $ \k -> k a
  as >>= f = table $ \k -> runTable as $ \a -> runTable (f a) k
  (>>) = (*>)
  fail _ = empty

instance MonadZip Table where
  -- we can handle this in a smarter fashion now
  mzipWith k m n = foldMap pure $ mzipWith k (Foldable.toList m) (Foldable.toList n)
  munzip m = (fmap fst m, fmap snd m) 

instance Alternative Table where
  empty = Table 0 $ \_ -> mempty
  Table m as <|> Table n bs = Table (m + n) $ \k -> as k `mappend` bs k

instance MonadPlus Table where
  mzero = Table 0 $ \_ -> mempty
  Table m as `mplus` Table n bs = Table (m + n) $ \k -> as k `mappend` bs k

instance MonadFix Table where
  mfix a2ba = foldMap pure $ mfix (Foldable.toList . a2ba)

filterMap :: (a -> Maybe b) -> Table a -> Table b
filterMap f m = table $ \k -> runTable m $ maybe mempty k . f
{-# INLINE filterMap #-}
