{-# LANGUAGE RankNTypes #-}
import Control.Arrow
import Control.Monad.Free.Church
import Control.Monad.ST
import Control.Monad.State
import Control.Monad.Trans.Class
import Data.Functor
import Data.Functor.Contravariant
import Data.Functor.Contravariant.Divisible
import Data.STRef

newtype Group a = Group
  { runGroup :: forall m b. Monad m => [(a,b)] -> (b -> m (b -> m ())) -> m ()
  }

instance Contravariant Group where  
  contramap f (Group m) = Group $ m . fmap (first f)

instance Divisible Group where
  conquer = Group $ \as k -> case as of
    [] -> return ()
    (_,b):bs -> do
      m <- k b
      forM_ bs $ m . snd
  -- divide f m n = Group $ \as k -> 

-- online nub
nubOf :: Group a -> [a] -> [a]
nubOf g as = runF (runGroup g [(a,a) | a <- as] $ \b -> liftF (b, \_ -> return ()))
  (const []) (uncurry (:))

-- offline group
groupOf :: Group a -> [a] -> [[a]]
groupOf g as = runST $ do
   xs <- execStateT (runGroup g [(a,a) | a <- as] go) []
   forM (reverse xs) $ \r -> do
     reverse <$> readSTRef r
  where
    go :: b -> StateT [STRef s [b]] (ST s) (b -> StateT [STRef s [b]] (ST s) ())
    go b = do
      rb <- lift $ newSTRef [b]
      modify (rb:)
      return $ \b' -> lift $ modifySTRef rb (b':)
