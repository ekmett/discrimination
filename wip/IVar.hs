{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE DeriveDataTypeable #-}

-- | Lazy promises

module Promise
  ( Lazy, runLazy, runLazy_
  , Promise(..)
  , promise, promise_
  , fulfill
  , demand
  ) where

import Control.Applicative
import Control.Concurrent.MVar
import Control.Exception
import Control.Monad (ap)
import Control.Monad.ST
import Control.Monad.ST.Class
import Control.Monad.ST.Unsafe
import Data.IORef
import Data.Type.Equality
import Unsafe.Coerce
import System.IO.Unsafe
import Data.Typeable

data BrokenPromise = BrokenPromise deriving (Show, Typeable)
instance Exception BrokenPromise

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

-- | A lazy I-Var.
data Promise s a where
  Promise :: MVar a -> a -> Promise s a

-- | Demand the result of an I-Var
demand :: Promise s a -> a
demand (Promise _ a) = a

type role Lazy nominal representational
newtype Lazy s a = Lazy { getLazy :: forall x. MVar (Maybe (IO (K s x))) -> IO (K s a) }


drive :: a -> MVar (Maybe (IO (K s x))) -> MVar a -> a
drive d = \mv v -> unsafePerformIO $ tryTakeMVar v >>= \case
  Just a -> return a -- if we're satisfied give the answer
  Nothing -> takeMVar mv >>= \case -- grab the lock on this computation
    Nothing -> do -- it has nothing left to do, so we fail to the default answer
      putMVar mv Nothing
      return d
    Just k -> tryTakeMVar v >>= \case -- ok, check to make sure we haven't been satisfied in the meantime
      Just a -> do
        putMVar mv (Just k) -- if so, restore the continuation, and return the answer
        return a
      Nothing -> do
        mk <- pump k v
        putMVar mv mk
        case mk of
          Nothing -> return d
          Just _  -> takeMVar v
  where
    pump :: IO (K s x) -> MVar a -> IO (Maybe (IO (K s x)))
    pump m v = m >>= \case
      Pure a        -> return Nothing
      Fulfilled u n -> case meq u v of
        Just Refl -> return (Just n)
        Nothing   -> pump n v
{-# NOINLINE drive #-}

-- | Promise that by the end of the computation we'll provide a "real" answer, or we'll fall back and give you this answer
promise :: a -> Lazy s (Promise s a)
promise d = Lazy $ \mv -> do
  v <- newEmptyMVar
  return $ Pure $ Promise v (drive d mv v)

-- | Promise that by the end of the computation we'll provide a "real" answer, or you'll get an error.
promise_ :: Lazy s (Promise s a)
promise_ = promise $ throw BrokenPromise

-- | Fulfill a promise.
fulfill :: Promise s a -> a -> Lazy s ()
fulfill (Promise v _) a = Lazy $ \ _ -> do
  putMVar v a
  return $ Fulfilled v $ return (Pure ())

instance Functor (Lazy s) where
  fmap f (Lazy m) = Lazy $ \mv -> fmap go (m mv) where
    go (Pure a)        = Pure (f a)
    go (Fulfilled v k) = Fulfilled v (fmap (fmap f) k)

instance Applicative (Lazy s) where
  pure = return
  (<*>) = ap

instance Monad (Lazy s) where
  return a = Lazy $ \_ -> return $ Pure a
  m >>= f = Lazy $ \mv -> let
      go mv (Pure a) = getLazy (f a) mv
      go mv (Fulfilled v k) = return $ Fulfilled v (k >>= go mv)
    in getLazy m mv >>= go mv

instance MonadST (Lazy s) where
  type World (Lazy s) = s
  liftST m = Lazy $ \_ -> Pure <$> unsafeSTToIO m

runLazy :: (forall s. Promise s a -> Lazy s ()) -> a -> a
runLazy f d = unsafePerformIO $ do
  mv <- newEmptyMVar
  v <- newEmptyMVar
  let iv = Promise v (drive d mv v)
  putMVar mv (Just (getLazy (f iv) mv))
  return $ demand iv

runLazy_ :: (forall s. Promise s a -> Lazy s ()) -> a
runLazy_ k = runLazy k $ throw BrokenPromise
