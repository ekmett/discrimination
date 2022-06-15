{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
module Utils where

import Data.Word (Word64)
import Data.Hashable (Hashable (..))
import GHC.Generics (Generic)
import Control.DeepSeq (NFData)
import Data.Discrimination (Grouping, Sorting)

import qualified Data.HashSet as HS
import qualified Data.Set as Set
import qualified System.Random.SplitMix as SM

import Control.Monad.ST (runST)
import qualified Data.Vector as V
import qualified Data.Vector.Algorithms.Intro as Intro
import qualified Data.Vector.Algorithms.Merge as Merge
import qualified Data.Vector.Algorithms.Radix as Radix


finiteList :: SM.SMGen -> Int -> [Word64]
finiteList gen n = take n $ go gen where
    go g = let (w,g') = SM.nextWord64 g in w : go g'

finiteListU :: SM.SMGen -> Int -> [UUID]
finiteListU gen n = take n $ go gen where
    go g0 = UUID w1 w2 : go g2 where
        (w1,g1) = SM.nextWord64 g0
        (w2,g2) = SM.nextWord64 g1

-- | Remove duplicates but keep elements in order.
--   O(n * log n)
--
-- From GHC's Util module
--
ordNub :: Ord a => [a] -> [a]
ordNub ys = go Set.empty ys
  where
    go _ [] = []
    go s (x:xs)
      | Set.member x s = go s xs
      | otherwise = x : go (Set.insert x s) xs

-- | Like 'ordNub' but using 'HashSet'.
hashNub :: (Eq a, Hashable a) => [a] -> [a]
hashNub ys = go HS.empty ys where
    go _ [] = []
    go s (x:xs)
        | HS.member x s = go s xs
        | otherwise     = x : go (HS.insert x s) xs

-- | Sort via vector.
introsort :: Ord a => [a] -> [a]
introsort xs = runST $ do
    v <- V.unsafeThaw (V.fromList xs)
    Intro.sort v
    V.toList <$> V.unsafeFreeze v

mergesort :: Ord a => [a] -> [a]
mergesort xs = runST $ do
    v <- V.unsafeThaw (V.fromList xs)
    Merge.sort v
    V.toList <$> V.unsafeFreeze v

radixsort :: Radix.Radix a => [a] -> [a]
radixsort xs = runST $ do
    v <- V.unsafeThaw (V.fromList xs)
    Radix.sort v
    V.toList <$> V.unsafeFreeze v

-------------------------------------------------------------------------------
-- UUID
-------------------------------------------------------------------------------

data UUID = UUID !Word64 !Word64
  deriving (Eq, Ord, Generic, Show, NFData, Hashable, Grouping, Sorting)
