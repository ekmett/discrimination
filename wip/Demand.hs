{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Demand where

import Data.Functor ((<$>))
import Control.Monad.Fix
import Control.Monad.Primitive
import Control.Monad.ST
import Data.Foldable (mapM_)
import Data.Functor.Contravariant
import Data.Functor.Contravariant.Divisible
import Data.Promise
import Data.Primitive.MutVar
import Prelude hiding (mapM_)

newtype Group a = Group
  { runGroup ::
    forall m b. PrimMonad m => (b -> m (b -> m ())) -> m (a -> b -> m (), m ())
  }

ungroup :: forall a b. Group a -> [(a,b)] -> [[b]]
ungroup (Group m) = \xs -> runLazy (go xs) [] where

  go :: [(a,b)] -> Promise s [[b]] -> Lazy s ()
  go xs p = do
     rp <- newMutVar p
     (f, stop) <- m (step rp)
     mapM_ (uncurry f) xs 
     stop

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


instance Contravariant Group

liftST :: PrimMonad m => ST (PrimState m) a -> m a
liftST = primToPrim

instance Divisible Group where
  conquer = Group $ \ (k :: b -> m (b -> m ())) -> liftST $ go <$> mfix (\v -> do newMutVar (start k v); return v) where
    start ::  PrimMonad m => (b -> m (b -> m ())) -> MutVar (PrimState m) (b -> m ()) -> b -> m ()
    start k r b = k b >>= writeMutVar r
    go :: PrimMonad m => MutVar (PrimState m) (b -> m ()) -> (a -> b -> m (), m ())
    go r = ( \ _ b -> readMutVar r >>= ($ b), return ())
    
  divide = undefined
    -- each group of m becomes a separate invocation of 'n' ?
    -- but we gather the results from them all by letting each of them push into the final result stream via an MutVar
