{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# OPTIONS_GHC -Wall -funbox-strict-fields -fno-warn-orphans -fno-warn-type-defaults -fno-full-laziness -O2 #-}
module Main where

import Control.Applicative
import Control.DeepSeq
import Control.Exception (evaluate)
import Control.Monad.ST hiding (runST)
import Criterion.Main
import Criterion.Types
import Data.Bits
import Data.Foldable
import Data.Functor
import Data.HashMap.Lazy (HashMap)
import Data.Maybe (fromMaybe)
import Data.Monoid
-- import Data.Primitive.Array
import Data.Traversable
import Data.Word
import qualified GHC.Exts as Exts
import Prelude hiding (lookup, length, foldr)
import qualified Data.IntMap as M
import qualified Data.HashMap.Lazy as H
import GHC.Types
import GHC.Base (realWorld#)
import GHC.ST hiding (runST, runSTRep)
import SmallArray

-- | Return the value computed by a state transformer computation.
-- The @forall@ ensures that the internal state used by the 'ST'
-- computation is inaccessible to the rest of the program.
runST :: (forall s. ST s a) -> a
runST st = runSTRep (case st of { ST st_rep -> st_rep })
{-# INLINE runST #-}

runSTRep :: (forall s. STRep s a) -> a
runSTRep st_rep = case st_rep realWorld# of
                        (# _, r #) -> r
{-# INLINE [0] runSTRep #-}

type Key = Word64
type Mask = Word16
type Offset = Int

pattern N = 16

ptrEq :: a -> a -> Bool
ptrEq x y = isTrue# (Exts.reallyUnsafePtrEquality# x y Exts.==# 1#)
{-# INLINEABLE ptrEq #-}

data WordMap v
  = Full !Key !Offset !(SmallArray (WordMap v))
  | Node !Key !Offset !Mask !(SmallArray (WordMap v))
  | Tip  !Key v
  | Nil
  deriving Show

node :: Key -> Offset -> Mask -> SmallArray (WordMap v) -> WordMap v
node k o 0xffff a = Full k o a
node k o m a      = Node k o m a
{-# INLINE node #-}

instance NFData v => NFData (WordMap v) where
  rnf (Full _ _ a)   = rnf a
  rnf (Node _ _ _ a) = rnf a
  rnf (Tip _ v) = rnf v
  rnf Nil = ()

instance Functor WordMap where
  fmap f = go where
    go (Full k o a) = Full k o (fmap go a)
    go (Node k o m a) = Node k o m (fmap go a)
    go (Tip k v) = Tip k (f v)
    go Nil = Nil
  {-# INLINEABLE fmap #-}

instance Foldable WordMap where
  foldMap f = go where
    go (Full _ _ a) = foldMap go a
    go (Node _ _ _ a) = foldMap go a
    go (Tip _ v) = f v
    go Nil = mempty
  {-# INLINEABLE foldMap #-}

instance Traversable WordMap where
  traverse f = go where
    go (Full k o a) = Full k o <$> traverse go a
    go (Node k o m a) = Node k o m <$> traverse go a
    go (Tip k v) = Tip k <$> f v
    go Nil = pure Nil
  {-# INLINEABLE traverse #-}

-- Note: 'level 0' will return a negative shift, don't use it
level :: Key -> Int
level w = 60 - (countLeadingZeros w .&. 0x7c)
{-# INLINE level #-}

maskBit :: Key -> Offset -> Int
maskBit k o = fromIntegral (unsafeShiftR k o .&. 0xf)
{-# INLINE maskBit #-}

mask :: Key -> Offset -> Word16
mask k o = unsafeShiftL 1 (maskBit k o)
{-# INLINE mask #-}

offset :: Int -> Word16 -> Int
offset k w = popCount $ w .&. (unsafeShiftL 1 k - 1)
{-# INLINE offset #-}

insert :: Key -> v -> WordMap v -> WordMap v
insert !k v xs0 = go xs0 where
  make o ok on = Node k o (mask k o .|. mask ok o) $ if k < ok then two (Tip k v) on else two on (Tip k v)
  go on@(Full ok n as)
    | o <- level (xor k ok), o > n = make o ok on
    | d <- maskBit k n, !oz <- indexSmallArray as d, !z  <- go oz, not (ptrEq z oz) = Full ok n (update16 d z as)
    | otherwise = on
  go on@(Node ok n m as)
    | o > n = make o ok on
    | not (testBit m d) = node ok n (unsafeShiftL 1 d .|. m) (insertSmallArray odm (Tip k v) as)
    | !oz <- indexSmallArray as odm, !z <- go oz, not (ptrEq z oz) = Node ok n m (updateSmallArray odm z as)
    | otherwise = on
    where
      o = level (xor k ok)
      d = maskBit k n
      odm = offset d m
  go on@(Tip ok ov)
    | k /= ok, o <- level (xor k ok) = make o ok on
    | ptrEq v ov = on
    | otherwise  = Tip k v
  go Nil = Tip k v
{-# INLINEABLE insert #-}

lookup :: Key -> WordMap v -> Maybe v
lookup !k (Full ok o a)
  | z <- unsafeShiftR (xor k ok) o, z <= 0xf = lookup k $ indexSmallArray a (fromIntegral z)
  | otherwise = Nothing
lookup k (Node ok o m a)
  | z <= 0xf && m .&. b /= 0 = lookup k (indexSmallArray a (popCount (m .&. (b - 1))))
  | otherwise = Nothing
  where
    z = unsafeShiftR (xor k ok) o
    b = unsafeShiftL 1 (fromIntegral z)
lookup k (Tip ok ov)
  | k == ok   = Just ov
  | otherwise = Nothing
lookup _ Nil = Nothing
{-# INLINEABLE lookup #-}

member :: Key -> WordMap v -> Bool
member !k (Full ok o a)
  | z <- unsafeShiftR (xor k ok) o = z <= 0xf && member k (indexSmallArray a (fromIntegral z))
member k (Node ok o m a)
  | z <- unsafeShiftR (xor k ok) o
  = z <= 0xf && let b = unsafeShiftL 1 (fromIntegral z) in
    m .&. b /= 0 && member k (indexSmallArray a (popCount (m .&. (b - 1))))
member k (Tip ok _) = k == ok
member _ Nil = False
{-# INLINEABLE member #-}

updateSmallArray :: Int -> a -> SmallArray a -> SmallArray a
updateSmallArray !k a i = runST $ do
  let n = length i
  o <- newSmallArray n undefined
  copySmallArray o 0 i 0 n
  writeSmallArray o k a
  unsafeFreezeSmallArray o
{-# INLINEABLE updateSmallArray #-}

update16 :: Int -> a -> SmallArray a -> SmallArray a
update16 !k a i = runST $ do
  o <- clone16 i
  writeSmallArray o k a
  unsafeFreezeSmallArray o
{-# INLINEABLE update16 #-}

insertSmallArray :: Int -> a -> SmallArray a -> SmallArray a
insertSmallArray !k a i = runST $ do
  let n = length i
  o <- newSmallArray (n + 1) a
  copySmallArray  o 0 i 0 k
  copySmallArray  o (k+1) i k (n-k)
  unsafeFreezeSmallArray o
{-# INLINEABLE insertSmallArray #-}

two :: a -> a -> SmallArray a
two !a !b = runST $ do
  arr <- newSmallArray 2 b
  writeSmallArray arr 0 a
  unsafeFreezeSmallArray arr
{-# INLINE two #-}

clone16 :: SmallArray a -> ST s (SmallMutableArray s a)
clone16 i = do
  o <- newSmallArray 16 undefined
  indexSmallArrayM i 0 >>= writeSmallArray o 0
  indexSmallArrayM i 1 >>= writeSmallArray o 1
  indexSmallArrayM i 2 >>= writeSmallArray o 2
  indexSmallArrayM i 3 >>= writeSmallArray o 3
  indexSmallArrayM i 4 >>= writeSmallArray o 4
  indexSmallArrayM i 5 >>= writeSmallArray o 5
  indexSmallArrayM i 6 >>= writeSmallArray o 6
  indexSmallArrayM i 7 >>= writeSmallArray o 7
  indexSmallArrayM i 8 >>= writeSmallArray o 8
  indexSmallArrayM i 9 >>= writeSmallArray o 9
  indexSmallArrayM i 10 >>= writeSmallArray o 10
  indexSmallArrayM i 11 >>= writeSmallArray o 11
  indexSmallArrayM i 12 >>= writeSmallArray o 12
  indexSmallArrayM i 13 >>= writeSmallArray o 13
  indexSmallArrayM i 14 >>= writeSmallArray o 14
  indexSmallArrayM i 15 >>= writeSmallArray o 15
  return o
{-# INLINE clone16 #-}

-- so we need to be able to quickly check if we differ on a higher nybble than the one
-- the word32s are the least and greatest keys possible in this node, not present

singleton :: Key -> v -> WordMap v
singleton !k v = Tip k v
{-# INLINE singleton #-}

fromList :: [(Word64,v)] -> WordMap v
fromList xs = foldl' (\r (k,v) -> insert k v r) Nil xs
{-# INLINE fromList #-}

main :: IO ()
main = do
    evaluate $ rnf [denseM, sparseM, sparseM']
    evaluate $ rnf [denseW, sparseW, sparseW']
    evaluate $ rnf [denseH, sparseH, sparseH']
    evaluate $ rnf [elems,  sElems,  sElemsSearch]
    evaluate $ rnf [keys,   sKeys, sKeysSearch]
    evaluate $ rnf [values, sValues]
    evaluate $ rnf [welems,  wsElems,  wsElemsSearch]
    evaluate $ rnf [wkeys,   wsKeys, wsKeysSearch]
    evaluate $ rnf [wvalues, wsValues]
    defaultMainWith (defaultConfig { timeLimit = 1 })
        [ bgroup "lookup"
            [ bgroup "present"
                [ bench "IntMap"  $ whnf (\m -> foldl' (\n k -> fromMaybe n (M.lookup k m)) 0 keys) denseM
                , bench "WordMap" $ whnf (\m -> foldl' (\n k -> fromMaybe n (lookup k m)) 0 wkeys) denseW
                , bench "HashMap" $ whnf (\m -> foldl' (\n k -> fromMaybe n (H.lookup k m)) 0 wkeys) denseH
                ]
            , bgroup "absent"
                [ bench "IntMap"  $ whnf (\m -> foldl' (\n k -> fromMaybe n (M.lookup k m)) 0 sKeysSearch) sparseM
                , bench "WordMap" $ whnf (\m -> foldl' (\n k -> fromMaybe n (lookup k m)) 0 wsKeysSearch) sparseW
                , bench "HashMap" $ whnf (\m -> foldl' (\n k -> fromMaybe n (H.lookup k m)) 0 wsKeysSearch) sparseH
                ]
            ]
        , bgroup "insert"
            [ bgroup "present"
                [ bench "IntMap"  $ whnf (\m0 -> foldl' (\m (k, v) -> M.insert k v m) m0 elems) denseM
                , bench "WordMap" $ whnf (\m0 -> foldl' (\m (k, v) -> insert k v m) m0 welems) denseW
                , bench "HashMap" $ whnf (\m0 -> foldl' (\m (k, v) -> H.insert k v m) m0 welems) denseH
                ]
            , bgroup "absent"
                [ bench "IntMap" $ whnf (\m0 -> foldl' (\m (k, v) -> M.insert k v m) m0 sElemsSearch) sparseM
                , bench "WordMap" $ whnf (\m0 -> foldl' (\m (k, v) -> insert k v m) m0 wsElemsSearch) sparseW
                , bench "HashMap" $ whnf (\m0 -> foldl' (\m (k, v) -> H.insert k v m) m0 wsElemsSearch) sparseH
                ]
            ]
        , bgroup "member"
            [ bgroup "present"
                [ bench "IntMap"  $ whnf (\m -> foldl' (\n x -> if M.member x m then n + 1 else n) (0 :: Int) keys) denseM
                , bench "WordMap" $ whnf (\m -> foldl' (\n x -> if member x m then n + 1 else n) (0 :: Int) wkeys) denseW
                , bench "HashMap" $ whnf (\m -> foldl' (\n x -> if H.member x m then n + 1 else n) (0 :: Int) wkeys) denseH
                ]
            , bgroup "absent"
                [ bench "IntMap"  $ whnf (\m -> foldl' (\n x -> if M.member x m then n + 1 else n) (0 :: Int) sKeysSearch) sparseM
                , bench "WordMap" $ whnf (\m -> foldl' (\n x -> if member x m then n + 1 else n) (0 :: Int) wsKeysSearch) sparseW
                , bench "HashMap" $ whnf (\m -> foldl' (\n x -> if H.member x m then n + 1 else n) (0 :: Int) wsKeysSearch) sparseH
                ]
            ]
        ]
  where
    denseM = M.fromAscList elems :: M.IntMap Int
    denseW = fromList welems :: WordMap Word64
    denseH = H.fromList welems :: HashMap Word64 Word64
    sparseM = M.fromAscList sElems :: M.IntMap Int
    sparseW = fromList wsElems :: WordMap Word64
    sparseH = H.fromList wsElems :: HashMap Word64 Word64
    sparseM' = M.fromAscList sElemsSearch :: M.IntMap Int
    sparseW' = fromList wsElemsSearch :: WordMap Word64
    sparseH' = H.fromList wsElemsSearch :: HashMap Word64 Word64

    elems = zip keys values
    keys = [1..2^12]
    values = [1..2^12]
    sElems = zip sKeys sValues
    sElemsSearch = zip sKeysSearch sValues
    sKeys = [1,3..2^12]
    sKeysSearch = [2,4..2^12]
    sValues = [1,3..2^12]

    welems = zip wkeys wvalues
    wkeys = [1..2^12]
    wvalues = [1..2^12]
    wsElems = zip wsKeys wsValues
    wsElemsSearch = zip wsKeysSearch wsValues
    wsKeys = [1,3..2^12]
    wsKeysSearch = [2,4..2^12]
    wsValues = [1,3..2^12]
