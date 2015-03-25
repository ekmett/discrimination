{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ParallelListComp #-}
module Data.Discrimination.Internal
  ( runs
  , groupNum
  , bdiscNat
  , updateBag
  , updateSet
  , spanEither
  ) where

import Data.Array as Array
import Data.Functor
import Data.Int
import qualified Data.List as List
import Prelude hiding (read, concat)

--------------------------------------------------------------------------------
-- * Utilities
--------------------------------------------------------------------------------

bdiscNat :: Int -> ([v] -> v -> [v]) -> [(Int,v)] -> [[v]]
bdiscNat n update xs = reverse <$> Array.elems (Array.accumArray update [] (0,n) xs)
{-# INLINE bdiscNat #-}

runs :: Eq a => [(a,b)] -> [[b]]
runs [] = []
runs ((a,b):xs0) = (b:ys0) : runs zs0
  where
    (ys0,zs0) = go xs0
    go [] = ([],[])
    go xs@((a', b'):xs')
      | a == a' = case go xs' of
         (ys, zs) -> (b':ys,zs)
      | otherwise = ([], xs)

groupNum :: [[k]] -> [(k,Int)]
groupNum kss = List.concat [ (,n) <$> ks | n <- [0..] | ks <- kss ]

updateBag :: [Int] -> Int -> [Int]
updateBag vs v = v : vs

updateSet :: [Int] -> Int -> [Int]
updateSet [] w = [w]
updateSet vs@(v:_) w
  | v == w    = vs
  | otherwise = w : vs

-- | Optimized and CPS'd version of 'Data.Either.partitionEithers', where all lefts are known to come before all rights
spanEither :: ([a] -> [b] -> c) -> [Either a b] -> c
spanEither k xs0 = go [] xs0 where
  go acc (Left x:xs) = go (x:acc) xs
  go acc rights = k (reverse acc) (fromRight <$> rights)

fromRight :: Either a b -> b
fromRight (Right y) = y
fromRight _ = error "unstable discriminator"
