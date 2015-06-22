{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Lazy where

import Control.Monad.ST.Class
import Data.Foldable (mapM_)
import Data.Functor.Contravariant
import Data.Functor.Contravariant.Divisible
import Data.Promise
import Data.STRef
import Prelude hiding (mapM_)

newtype Group a = Group
  { runGroup ::
    forall m b. MonadST m => [(a,b)] -> (b -> m (b -> m ())) -> m () -- CPS this?
  }

ungroup :: forall a b. Group a -> [(a,b)] -> [[b]]
ungroup (Group m) = \xs -> runLazy (go xs) [] where

  go :: [(a,b)] -> Promise s [[b]] -> Lazy s ()
  go xs p = do
     rp <- liftST (newSTRef p)
     m xs (step rp)

  step :: STRef s (Promise s [[b]]) -> b -> Lazy s (b -> Lazy s ())
  step rp b = do
    p <- liftST $ readSTRef rp
    q <- promise [] 
    p' <- promise []
    p != (b : demand q) : demand p'
    liftST $ writeSTRef rp p'
    rq <- liftST $ newSTRef q
    return $ \b' -> do
      q' <- liftST $ readSTRef rq
      q'' <- promise []
      q' != b' : demand q''
      liftST $ writeSTRef rq q''

instance Contravariant Group

instance Divisible Group where
  conquer = Group go where
    go :: MonadST m => [(a,b)] -> (b -> m (b -> m ())) -> m ()
    go [] _ = return ()
    go ((_,b):bs) k = do
      m <- k b
      mapM_ (m . snd) bs
    
  divide (f :: a -> (b,c)) m n = Group $ \ads k -> runGroup m [ (b, (c, d)) | (a,d) <- ads, let (b, c) = f a ] (step1 k) where
    step1 :: MonadST m => (d -> m (d -> m ())) -> (c, d) -> m ((c, d) -> m ())
    step1 k (c, d) = undefined -- stuck

    -- each group of m becomes a separate invocation of 'n' ?
    -- but we gather the results from them all by letting each of them push into the final result stream via an STRef
