{-# LANGUAGE GADTs, TypeOperators, RankNTypes, DeriveDataTypeable, DefaultSignatures, FlexibleContexts #-}
{-# LANGUAGE ForeignFunctionInterface, UnliftedFFITypes, MagicHash #-}
{-# LANGUAGE RoleAnnotations #-}
{-# OPTIONS_GHC -rtsopts -threaded -fno-cse -fno-full-laziness #-}

module Data.Discrimination.Type
  ( Disc(..)
  , descending
  , discNat
  , discShort
  , sdiscNat
  ) where

import Control.Arrow
import Control.Concurrent
import Control.Monad
import Data.Functor
import Data.Functor.Contravariant
import Data.Functor.Contravariant.Divisible
import Data.Int
import Data.IORef
import Data.Monoid
import Data.Typeable
import Data.Void
import Data.Vector.Mutable as MV
import Foreign.C.Types
import GHC.Prim
import Prelude hiding (read)
import System.IO.Unsafe

-- | Discriminator
--
-- TODO: use [(a,b)] -> [NonEmpty b] to better indicate safety?
newtype Disc a = Disc { (%) :: forall b. [(a,b)] -> [[b]] }
  deriving Typeable

type role Disc representational

infixr 9 %

instance Contravariant Disc where
  contramap f (Disc g) = Disc $ g . map (first f)

instance Divisible Disc where
  conquer = Disc $ return . fmap snd
  divide k (Disc l) (Disc r) = Disc $ \xs ->
    l [ (b, (c, d)) | (a,d) <- xs, let (b, c) = k a] >>= r 

instance Decidable Disc where
  lose k = Disc $ fmap (absurd.k.fst) 
  choose f (Disc l) (Disc r) = Disc $ \xs -> let ys = fmap (first f) xs
    in l [ (k,v) | (Left k, v) <- ys]
    ++ r [ (k,v) | (Right k, v) <- ys]

instance Monoid (Disc a) where
  mempty = conquer
  mappend (Disc l) (Disc r) = Disc $ \xs -> l [ (fst x, x) | x <- xs ] >>= r

descending :: Disc a -> Disc a
descending (Disc l) = Disc (reverse . l)

-- | unordered discrimination by bucket, not inherently thread-safe
-- but it only has to restore the elements that were used
-- unlike the more obvious ST implementation, so it wins by
-- a huge margin in a race, especially when we have a large
-- keyspace, sparsely used, with low multi-threaded contention.
discNat :: Int -> Disc Int
discNat n = Disc $ unsafePerformIO $ do
  t <- MV.replicate n []
  lock <- newMVar () -- TODO have this hold onto a list of arrays like 't' and take 1
  let step1 keys (k, v) = read t k >>= \vs -> case vs of
        [] -> (k:keys) <$ write t k [v]
        _  -> keys     <$ write t k (v:vs)
      step2 vss k = do
        elems <- read t k
        (reverse elems : vss) <$ write t k []
      go xs = unsafePerformIO $ do
        takeMVar lock
        ys <- foldM step1 [] xs
        zs <- foldM step2 [] ys
        zs <$ putMVar lock () -- put the array back on the lazy list
      {-# NOINLINE go #-}
  return go
{-# NOINLINE discNat #-}

-- | discriminate by bucket, output ordered
sdiscNat :: Int -> Disc Int
sdiscNat = error "TODO"

-- | Shared bucket set for small integers
discShort :: Disc Int
discShort = discNat 65536
{-# NOINLINE discShort #-}

discIORef :: Disc (IORef a)
discIORef = undefined

foreign import ccall unsafe "disc.h c_snapshotArray" snapshotArray :: Array# a -> MutableByteArray# RealWorld -> CInt -> IO ()
