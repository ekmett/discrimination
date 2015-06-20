{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

-- working towards a demand driven IVar model
module IVar where

import Control.Applicative
import Control.Concurrent.MVar
import Control.Monad (ap)
import Data.IORef
import Data.Type.Equality
import Unsafe.Coerce
import System.IO.Unsafe

meq :: forall a b. MVar a -> MVar b -> Maybe (a :~: b)
meq a b = case unsafeCoerce Refl :: a :~: b of
  Refl | a == b -> Just Refl
       | otherwise -> Nothing

data K s a where
  Pure      :: a -> K s a
  Fulfilled :: MVar x -> IO (K s a) -> K s a

instance Functor (K s) where
  fmap f (Pure a) = Pure (f a)
  fmap f (Fulfilled m k) = Fulfilled m (fmap (fmap f) k)

data IVar s a where
  IVar :: MVar (IO (K s x)) -> MVar a -> a -> IVar s a

demand :: IVar s a -> a
demand (IVar _ _ a) = a

newtype I s a = I { unI :: forall x. MVar (IO (K s x)) -> IO (K s a) }

fulfill :: IVar s a -> a -> I s ()
fulfill (IVar _ v _) a = I $ \ _ -> do
  putMVar v a
  return $ Fulfilled v $ return (Pure ())

newIVar :: I s (IVar s a)
newIVar = I $ \mv -> do
  v <- newEmptyMVar
  return $ Pure $ IVar mv v (drive mv v)

drive :: MVar (IO (K s x)) -> MVar a -> a
drive = \mv v -> unsafePerformIO $ tryTakeMVar v >>= \case
  Just a -> return a
  Nothing -> do
    k <- takeMVar mv
    k' <- pump k v
    putMVar mv k'
    takeMVar v
  where
    pump :: IO (K s x) -> MVar a -> IO (IO (K s x))
    pump m v = m >>= \case
      Pure a -> error "pump: unfulfilled promise"
      Fulfilled u k -> case meq u v of
        Just Refl -> return k
        Nothing   -> pump k v
{-# NOINLINE drive #-}

instance Functor (I s) where
  fmap f (I m) = I $ \mv -> fmap go (m mv) where
    go (Pure a)        = Pure (f a)
    go (Fulfilled v k) = Fulfilled v (fmap (fmap f) k)

instance Applicative (I s) where
  pure = return
  (<*>) = ap

instance Monad (I s) where
  return a = I $ \_ -> return $ Pure a
  m >>= f = I $ \mv ->
    let go mv (Pure a) = unI (f a) mv
        go mv (Fulfilled v k) = return $ Fulfilled v (k >>= go mv)
    in unI m mv >>= go mv

runI :: (forall s. IVar s a -> I s ()) -> a
runI f = unsafePerformIO $ do
    mv <- newEmptyMVar
    v <- newEmptyMVar
    let iv = IVar mv v (drive mv v)
    putMVar mv (unI (f iv) mv)
    return $ demand iv
