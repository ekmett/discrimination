{-# LANGUAGE CPP #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Data.Discrimination.Join
  ( Join(..)
  , inner, outer, leftOuter, rightOuter
  , Mode(..)
  ) where

import Control.Applicative
import Control.Arrow
import Control.Monad
import Data.Data
import Data.Discrimination.Class
import Data.Discrimination.Table
import Data.Foldable as Foldable
import Data.Functor.Contravariant
import Data.Functor.Contravariant.Divisible
import Data.IORef (newIORef, atomicModifyIORef)
import Data.Int
import Data.Monoid hiding (Any)
import GHC.Arr (Ix(..))
import GHC.Exts
import Prelude hiding (read, null, reverse)
import System.IO.Unsafe
import Unsafe.Coerce
import qualified Data.Vector.Mutable as UM
{-
import Data.Void
import Data.Array as Array
import GHC.Prim (Any, RealWorld)
-}

data Mode = Inner | Outer | LeftOuter | RightOuter
  deriving (Eq,Ord,Show,Read,Bounded,Ix,Enum,Typeable,Data)

-- | Linear time table inner joins
newtype Join d = Join { runJoin :: forall a b c. Mode -> (Table a -> Table b -> c) -> (a -> d) -> (b -> d) -> Table a -> Table b -> [c] }
  deriving Typeable

instance Contravariant Join where
  contramap ed (Join m) = Join $ \mode abc ae be -> m mode abc (ed.ae) (ed.be)

instance Divisible Join where
  conquer = Join $ \_ k _ _ as bs -> [k as bs] -- the trivial join
  divide def (Join me) (Join mf) = Join $ \ mode abc ad bd as bs -> join $ me
    mode
    (mf mode (\as' bs' -> abc (fmap snd as') (fmap snd bs')) (snd.fst) (snd.fst))
    (fst.fst) (fst.fst)
    (fmap (def.ad &&& id) as)
    (fmap (def.bd &&& id) bs)

instance Decidable Join where
  lose _ = Join $ \_ _ _ _ _ _ -> [] -- an impossible join
  choose def (Join me) (Join mf) = Join $ \ mode abc ad bd as bs ->
    let (eas, fas) = foldMap (\a -> case def (ad a) of Left e -> (pure (e, a), mempty); Right f -> (mempty, pure (f, a))) as
        (ebs, fbs) = foldMap (\b -> case def (bd b) of Left e -> (pure (e, b), mempty); Right f -> (mempty, pure (f, b))) bs
    in me mode (\eas' ebs' -> abc (fmap snd eas') (fmap snd ebs')) fst fst eas ebs
    ++ mf mode (\fas' fbs' -> abc (fmap snd fas') (fmap snd fbs')) fst fst fas fbs  

short :: Int
short = 65556

-- | Shared bucket set for small integers
instance Grouped Join where
  discShort = unsafePerformIO $ do
    ts <- newIORef ([] :: [UM.MVector RealWorld ([Any],[Any])])
    return $ Join $ go ts
    where
      -- inner joins
      inner1 t ad keys v 
        | k <- ad v = UM.read t k >>= \case
          ([], []) -> (k:keys) <$ UM.write t k ([v],[])
          (vs, us) -> keys     <$ UM.write t k (v:vs, us)
      inner2 t bd keys v 
        | k <- bd v = UM.read t k >>= \case
          (us@(_:_),[]) -> (k:keys) <$ UM.write t k (us,[v])
          (us@(_:_),vs) -> keys <$ UM.write t k (us,v:vs)
          _             -> return keys

      -- outer joins
      outer1 t ad keys v 
        | k <- ad v = UM.read t k >>= \case
          ([], []) -> (k:keys) <$ UM.write t k ([v],  [])
          (vs, us) -> keys     <$ UM.write t k (v:vs, us)
      outer2 t bd keys v
        | k <- bd v = UM.read t k >>= \case
          ([], []) -> (k:keys) <$ UM.write t k ([], [v] )
          (us, vs) -> keys     <$ UM.write t k (us, v:vs)

      -- left outer joins
      left1 t ad keys v 
        | k <- ad v = UM.read t k >>= \case
          ([], []) -> (k:keys) <$ UM.write t k ([v],  [])
          (vs, us) -> keys     <$ UM.write t k (v:vs, us)
      left2 t bd v
        | k <- bd v = UM.read t k >>= \case
          ([], _ ) -> return ()
          (us, vs) -> UM.write t k (us, v:vs)

      -- right outer joins
      right1 t bd keys v
        | k <- bd v = UM.read t k >>= \case
          ([], []) -> (k:keys) <$ UM.write t k ([], [v] )
          (us, vs) -> keys     <$ UM.write t k (us, v:vs)
      right2 t ad v
        | k <- ad v = UM.read t k >>= \case
          (_ , []) -> return ()
          (vs, us) -> UM.write t k (v:vs, us)

        
      -- cleanup
      cleanup t abc vss k = do
        (es,fs) <- UM.read t k
        (abc (reverse $ fromList es) (reverse $ fromList fs) : vss) <$ UM.write t k ([],[])

      cleanup_ t k = UM.write t k ([],[])

      -- cleanup without cleanup
      gather t abc vss k = do
        (es, fs) <- UM.read t k
        return $ abc (reverse $ fromList es) (reverse $ fromList fs) : vss

      go ts mode abc ad bd ls rs = unsafePerformIO $ do
        mt <- atomicModifyIORef ts $ \case
          (y:ys) -> (ys, Just y)
          []     -> ([], Nothing)
        t <- maybe (UM.replicate short ([],[])) (return . unsafeCoerce) mt
        -- inner join
        js <- case mode of
          Inner -> do
            mess <- foldlM (inner1 t ad) [] ls
            keys <- foldlM (inner2 t bd) [] rs
            js <- foldM (gather t abc) [] keys
            traverse_ (cleanup_ t) mess
            return js
          Outer -> do
            xs <- foldlM (outer1 t ad) [] ls
            lrs <- foldlM (outer2 t bd) xs rs
            foldM (cleanup t abc) [] lrs
          LeftOuter -> do
            xs <- foldlM (left1 t ad) [] ls
            traverse_ (left2 t bd) rs
            foldM (cleanup t abc) [] xs
          RightOuter -> do
            xs <- foldlM (right1 t bd) [] rs
            traverse_ (right2 t ad) ls
            foldM (cleanup t abc) [] xs
        atomicModifyIORef ts $ \ts0 -> (unsafeCoerce t:ts0, ())
        return js
      {-# NOINLINE go #-}
  {-# NOINLINE discShort #-}

-- TODO: instance Orderable Join where

inner
  :: Grouping d
  => (a -> b -> c)
  -> (a -> d)
  -> (b -> d)
  -> Table a
  -> Table b
  -> [Table c]
inner f = runJoin grouping Inner (liftA2 f) 

outer
  :: Grouping d
  => (a -> b -> c)
  -> (a -> c)
  -> (b -> c) 
  -> (a -> d)
  -> (b -> d)
  -> Table a
  -> Table b
  -> [Table c]
outer f g h = runJoin grouping Outer $ \ls rs ->
  if null rs
  then g <$> ls
  else if null ls
       then h <$> rs
       else f <$> ls <*> rs

leftOuter
  :: Grouping d
  => (a -> b -> c)
  -> (a -> c)
  -> (a -> d)
  -> (b -> d)
  -> Table a
  -> Table b
  -> [Table c]
leftOuter f g = runJoin grouping LeftOuter $ \ls rs ->
  if null rs 
  then g <$> ls
  else f <$> ls <*> rs

rightOuter
  :: Grouping d
  => (a -> b -> c)
  -> (b -> c)
  -> (a -> d)
  -> (b -> d)
  -> Table a
  -> Table b
  -> [Table c]
rightOuter f g = runJoin grouping RightOuter $ \ls rs ->
  if null ls 
  then g <$> rs
  else f <$> ls <*> rs
