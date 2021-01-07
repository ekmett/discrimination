{-# LANGUAGE CPP #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE MagicHash #-}
module Data.Discrimination.Internal
  ( runs
  , groupNum
  , bdiscNat
  , updateBag
  , updateSet
  , spanEither
  , integerCases
  , naturalCases
  ) where

import Data.Array as Array
import Data.Functor
import Data.Int
import qualified Data.List as List
import Prelude hiding (read, concat)

import GHC.Word
import GHC.Exts
import Data.Primitive.Types (Prim)
import Data.Primitive.PrimArray

#ifdef MIN_VERSION_ghc_bignum
import GHC.Num.Integer
import GHC.Num.Natural
#else
import GHC.Natural
import GHC.Integer.GMP.Internals
#endif

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

-------------------------------------------------------------------------------
-- * Integer and Natural
-------------------------------------------------------------------------------

integerCases :: Integer -> Either (Int,[Word]) (Either Int (Int,[Word]))
#ifdef MIN_VERSION_ghc_bignum
integerCases (IN b) = Left          $ decomposeBigNat b
integerCases (IS i) = Right . Left  $ I# i
integerCases (IP b) = Right . Right $ decomposeBigNat b
#else
integerCases (Jn# b) = Left          $ decomposeBigNat b
integerCases (S#  i) = Right . Left  $ I# i
integerCases (Jp# b) = Right . Right $ decomposeBigNat b
#endif
{-# INLINE integerCases #-}

naturalCases :: Natural -> Either Word (Int,[Word])
#ifdef MIN_VERSION_ghc_bignum
naturalCases (NS w) = Left $ W# w
naturalCases (NB b) = Right $ decomposeBigNat b
#else
naturalCases (NatS# w) = Left $ W# w
naturalCases (NatJ# b) = Right $ decomposeBigNat b
#endif
{-# INLINE naturalCases #-}

-- We need to reverse the limb array. Its stored least-significant word first
-- but for comparasion to work right we need most-significant words first.
#ifdef MIN_VERSION_ghc_bignum
decomposeBigNat :: ByteArray# -> (Int, [Word])
decomposeBigNat ba = let pa = PrimArray ba :: PrimArray Word in (sizeofPrimArray pa, primArrayToReverseList pa)
#else
decomposeBigNat :: BigNat -> (Int, [Word])
decomposeBigNat (BN# ba) = let pa = PrimArray ba :: PrimArray Word in (sizeofPrimArray pa, primArrayToReverseList pa)
#endif
{-# INLINE decomposeBigNat #-}

primArrayToReverseList :: Prim a => PrimArray a -> [a]
primArrayToReverseList xs = build (\c n -> foldlPrimArray (flip c) n xs)
{-# INLINE primArrayToReverseList #-}
