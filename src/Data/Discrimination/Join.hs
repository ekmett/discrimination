{-# LANGUAGE CPP #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
module Data.Discrimination.Join
  ( Join(..)
  ) where

import Control.Applicative
import Control.Arrow
import Control.Monad
-- import Data.Array as Array
import Data.Discrimination.Class
import Data.Discrimination.Table
import Data.Foldable as Foldable
import Data.Functor.Contravariant
import Data.Functor.Contravariant.Divisible
import Data.IORef (newIORef, atomicModifyIORef)
import Data.Int
import Data.Monoid hiding (Any)
-- import Data.Typeable
-- import Data.Void
import GHC.Exts
-- import GHC.Prim (Any, RealWorld)
import Prelude hiding (read)
import System.IO.Unsafe
import Unsafe.Coerce
import qualified Data.Vector.Mutable as UM



-- | Linear time table inner joins
newtype Join d = Join { runJoin :: forall a b c. (Table a -> Table b -> c) -> (a -> d) -> (b -> d) -> Table a -> Table b -> [c] }

instance Contravariant Join where
  contramap ed (Join m) = Join $ \abc ae be -> m abc (ed.ae) (ed.be)

instance Divisible Join where
  conquer = Join $ \k _ _ as bs -> [k as bs] -- the trivial join
  divide def (Join me) (Join mf) = Join $ \abc ad bd as bs -> join $ me
    (mf (\as' bs' -> abc (fmap snd as') (fmap snd bs')) (snd.fst) (snd.fst))
    (fst.fst) (fst.fst)
    (fmap (def.ad &&& id) as)
    (fmap (def.bd &&& id) bs)

instance Decidable Join where
  lose _ = Join $ \_ _ _ _ _ -> [] -- an impossible join
  choose def (Join me) (Join mf) = Join $ \abc ad bd as bs ->
    let (eas, fas) = foldMap (\a -> case def (ad a) of Left e -> (pure (e, a), mempty); Right f -> (mempty, pure (f, a))) as
        (ebs, fbs) = foldMap (\b -> case def (bd b) of Left e -> (pure (e, b), mempty); Right f -> (mempty, pure (f, b))) bs
    in me (\eas' ebs' -> abc (fmap snd eas') (fmap snd ebs')) fst fst eas ebs
    ++ mf (\fas' fbs' -> abc (fmap snd fas') (fmap snd fbs')) fst fst fas fbs  

short :: Int
short = 65556

-- | Shared bucket set for small integers
instance Disorderable Join where
  discShort = unsafePerformIO $ do
    ts <- newIORef ([] :: [UM.MVector RealWorld ([Any],[Any])])
    return $ Join $ go ts
    where
      step1 t ad v      | k <- ad v = UM.read t k >>= \(vs,[]) -> UM.write t k (v:vs,[])
      step2 t bd keys v | k <- bd v = UM.read t k >>= \case  -- (us,vs) -> case vs of
        (us@(_:_),[]) -> (k:keys) <$ UM.write t k (us,[v])
        (us@(_:_),vs) -> keys <$ UM.write t k (us,v:vs)
        _             -> return keys
      step3 t abc vss k = do
        (es,fs) <- UM.read t k
        (abc (reverseTable $ fromList es) (reverseTable $ fromList fs) : vss) <$ UM.write t k ([],[])
      go ts abc ad bd ls rs = unsafePerformIO $ do
        mt <- atomicModifyIORef ts $ \case
          (y:ys) -> (ys, Just y)
          []     -> ([], Nothing)
        t <- maybe (UM.replicate short ([],[])) (return . unsafeCoerce) mt
        traverse_ (step1 t ad) ls
        lrs <- foldlM (step2 t bd) [] rs
        js  <- foldM (step3 t abc) [] lrs
        atomicModifyIORef ts $ \ts0 -> (unsafeCoerce t:ts0, ())
        return js
      {-# NOINLINE go #-}
  {-# NOINLINE discShort #-}

-- instance Orderable Join where
