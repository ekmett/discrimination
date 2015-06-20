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
  
data K a where 
  Pure      :: a -> K a
  Fulfilled :: MVar x -> IO (K a) -> K a

instance Functor K where
  fmap f (Pure a) = Pure (f a)
  fmap f (Fulfilled m k) = Fulfilled m (fmap (fmap f) k)

data IVar a where
  IVar :: MVar (IO (K x)) -> MVar a -> a -> IVar a

demand :: IVar a -> a
demand (IVar _ _ a) = a

newtype I a = I { runI :: forall x. MVar (IO (K x)) -> IO (K a) }

fulfill :: IVar a -> a -> I ()
fulfill (IVar _ v _) a = I $ \ _ -> do
  putMVar v a
  return $ Fulfilled v $ return (Pure ())

newIVar :: I (IVar a)
newIVar = I $ \mv -> do
  v <- newEmptyMVar
  return $ Pure $ IVar mv v $ drive mv v where

    drive :: MVar (IO (K x)) -> MVar a -> a
    drive mv v = unsafePerformIO $ tryTakeMVar v >>= \case
      Just a -> return a
      Nothing -> do
        k <- takeMVar mv
        k' <- pump k v
        putMVar mv k'
        takeMVar v
    {-# NOINLINE drive #-}

    pump :: IO (K x) -> MVar a -> IO (IO (K x))
    pump m v = m >>= \case
      Pure a -> error "pump: unfulfilled promise"
      Fulfilled u k -> case meq u v of
        Just Refl -> return k
        Nothing   -> pump k v
  
instance Functor I where
  fmap f (I m) = I $ \mv -> fmap go (m mv) where
    go (Pure a)        = Pure (f a)
    go (Fulfilled v k) = Fulfilled v (fmap (fmap f) k)

instance Applicative I where
  pure = return
  (<*>) = ap

instance Monad I where
  return a = I $ \_ -> return $ Pure a
  m >>= f = I $ \mv -> 
    let go mv (Pure a) = runI (f a) mv
        go mv (Fulfilled v k) = return $ Fulfilled v (k >>= go mv)
    in runI m mv >>= go mv

runI :: I x -> x
runI (I m) = unsafePerformIO $ do
  mv <- newEmptyMVar
  putMVar mv (m mv)
  -- we need a better way to give something back!
  

newtype I a = I { runI :: forall x. MVar (IO (K x)) -> IO (K a) }
