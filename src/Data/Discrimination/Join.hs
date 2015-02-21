{-# LANGUAGE CPP #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
module Data.Discrimination.Join
  ( Join(..)
--   , Joinable(..)
  ) where

import Control.Applicative
import Control.Arrow
import Control.Monad
-- import Data.Discrimination.Generic
import Data.Discrimination.Table
import Data.Foldable as Foldable
import Data.Functor.Contravariant
import Data.Functor.Contravariant.Divisible
import Data.Monoid

-- | Linear time table joins
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

-- TODO: retire these by abstracting over discNat and sdiscNat
        
{-
class Joinable a where
  join :: Join a
  default join :: Deciding Joinable a => Join a
  join = deciding (Proxy :: Proxy Joinable) join

instance Joinable Void
instance Joinable ()
instance Joinable Bool
instance Joinable Ordering
instance Joinable a => Joinable [a]
instance Joinable a => Joinable (Maybe a)
instance (Joinable a, Joinable b) => Joinable (Either a b)
instance (Joinable a, Joinable b) => Joinable (a, b)

class Joinable1 f where
  join1 :: Join a -> Join (f a)
  default join1 :: Deciding1 Joinable f => Join a -> Join (f a)
  join1 = deciding1 (Proxy :: Proxy Joinable) join

instance Joinable1 []
instance Joinable1 Maybe
instance Joinable a => Joinable1 (Either a)
instance Joinable a => Joinable1 ((,) a)
-}

{-
innerJoin :: (a -> b -> c) -> Table a -> Table b -> Table c
innerJoin k as bs
  | count as == 0 || count bs == 0 =

leftJoin :: (a -> Maybe b -> c) -> Table a -> Table b -> Table c
leftJoin k as bs 
  | count bs == 0 = fmap (`k` Nothing) as
  | otherwise     = liftA2 (\a b -> k a $ Just b) as bs

rightJoin :: (Maybe a -> b -> c) -> Table a -> Table b -> Table c
rightJoin k as bs 
  | count as == 0 = fmap (k Nothing) bs
  | otherwise     = liftA2 (k . Just) as bs

outerJoin :: (These a b -> c) -> Table a -> Table b -> Table c
outerJoin 
-}
