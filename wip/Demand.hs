{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
module Demand where

import Data.Functor ((<$>))
import Control.Monad (join)
import Control.Monad.Fix
import Control.Monad.Primitive
import Control.Monad.ST
import Data.Foldable (mapM_)
import Data.Functor.Contravariant
import Data.Functor.Contravariant.Divisible
import Data.Promise
import Data.Primitive.MutVar
import Data.Void
import qualified Data.Vector.Mutable as UM
import Prelude hiding (mapM_)

newtype Group a = Group { runGroup :: forall m b. PrimMonad m => (b -> m (b -> m ())) -> m (a -> b -> m ()) }

ungroup :: forall a b. Group a -> [(a,b)] -> [[b]]
ungroup (Group m) = \xs -> runLazy (go xs) [] where

  go :: [(a,b)] -> Promise s [[b]] -> Lazy s ()
  go xs p = do
     rp <- newMutVar p
     f <- m (step rp)
     mapM_ (uncurry f) xs

  step :: MutVar s (Promise s [[b]]) -> b -> Lazy s (b -> Lazy s ())
  step rp b = do
    p <- readMutVar rp
    q <- promise []
    p' <- promise []
    p != (b : demand q) : demand p'
    writeMutVar rp p'
    rq <- newMutVar q
    return $ \b' -> do
      q' <- readMutVar rq
      q'' <- promise []
      q' != b' : demand q''
      writeMutVar rq q''

instance Contravariant Group where
  contramap f m = Group $ \k -> do
    g <- runGroup m k
    return (g . f)

liftST :: PrimMonad m => ST (PrimState m) a -> m a
liftST = primToPrim

instance Divisible Group where
  conquer = Group $ \ (k :: b -> m (b -> m ())) -> do
    v <- newMutVar undefined
    writeMutVar v $ \b -> k b >>= writeMutVar v
    return $ \ _ b -> readMutVar v >>= ($ b)

  divide f m n = Group $ \k -> do
    kbcd <- runGroup m $ \ (c, d) -> do
      kcd <- runGroup n k
      kcd c d
      return $ uncurry kcd
    return $ \ a d -> case f a of
      (b, c) -> kbcd b (c, d)

instance Decidable Group where
  choose f m n = Group $ \k -> do
    kb <- runGroup m k
    kc <- runGroup n k
    return (either kb kc . f)

  lose k = Group $ \_ -> return (absurd . k)

-- | Perform productive stable unordered discrimination by bucket.
groupingNat :: Int -> Group Int
groupingNat = \ n -> Group $ \k -> do
  t <- UM.replicate n Nothing
  return $ \ a b -> UM.read t a >>= \case
    Nothing -> k b >>= UM.write t a . Just
    Just k' -> k' b
