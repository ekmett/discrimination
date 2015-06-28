{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# OPTIONS_GHC -Wall -funbox-strict-fields -fno-warn-orphans -fno-warn-type-defaults -O2 #-}
#ifdef ST_HACK
{-# OPTIONS_GHC -fno-full-laziness #-}
#endif
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
import Data.Traversable
import Data.Word
import qualified GHC.Exts as Exts
import Prelude hiding (lookup, length, foldr)
import qualified Data.IntMap as M
import qualified Data.HashMap.Lazy as H
import GHC.Types
import GHC.Base (realWorld#)
import SmallArray

#ifndef ST_HACK

import GHC.ST

#else

import GHC.ST hiding (runST, runSTRep)

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

#endif

type Key = Word64
type Mask = Word32
type Offset = Int

pattern N = 16

ptrEq :: a -> a -> Bool
ptrEq x y = isTrue# (Exts.reallyUnsafePtrEquality# x y Exts.==# 1#)
{-# INLINEABLE ptrEq #-}

ptrNeq :: a -> a -> Bool
ptrNeq x y = isTrue# (Exts.reallyUnsafePtrEquality# x y Exts./=# 1#)
{-# INLINEABLE ptrNeq #-}

data WordMap v
  = Full !Key !Offset !(SmallArray (WordMap v))
  | Node !Key !Offset !Mask !(SmallArray (WordMap v))
  | Fork !Key !Key !Offset !(WordMap v) !(WordMap v)
  | Tip  !Key v
  | Nil
  deriving Show

node :: Key -> Offset -> Mask -> SmallArray (WordMap v) -> WordMap v
node k o 0xffffffff a = Full k o a
node k o m a       = Node k o m a
{-# INLINE node #-}

instance NFData v => NFData (WordMap v) where
  rnf (Full _ _ a)   = rnf a
  rnf (Node _ _ _ a) = rnf a
  rnf (Fork _ _ _ a b) = rnf a `seq` rnf b
  rnf (Tip _ v)      = rnf v
  rnf Nil = ()

instance Functor WordMap where
  fmap f = go where
    go (Full k o a) = Full k o (fmap go a)
    go (Node k o m a) = Node k o m (fmap go a)
    go (Fork k k' o v v') = Fork k k' o (go v) (go v')
    go (Tip k v) = Tip k (f v)
    go Nil = Nil
  {-# INLINEABLE fmap #-}

instance Foldable WordMap where
  foldMap f = go where
    go (Full _ _ a) = foldMap go a
    go (Node _ _ _ a) = foldMap go a
    go (Fork _ _ _ v v') = go v `mappend` go v'
    go (Tip _ v) = f v
    go Nil = mempty
  {-# INLINEABLE foldMap #-}

instance Traversable WordMap where
  traverse f = go where
    go (Full k o a) = Full k o <$> traverse go a
    go (Node k o m a) = Node k o m <$> traverse go a
    go (Fork k k' o v v') = Fork k k' o <$> go v <*> go v'
    go (Tip k v) = Tip k <$> f v
    go Nil = pure Nil
  {-# INLINEABLE traverse #-}

-- >>> level . bit <$> [0..63]
-- [0,0,0,0,5,5,5,5,5,10,10,10,10,10,15,15,15,15,15,20,20,20,20,20,25,25,25,25,25,30,30,30,30,30,35,35,35,35,35,40,40,40,40,40,45,45,45,45,45,50,50,50,50,50,55,55,55,55,55,60,60,60,60,60]

-- better would be
-- [0,0,0,0,0,5,5,5,5,5,10,10,10,10,10,15,15,15,15,15,20,20,20,20,20,25,25,25,25,25,30,30,30,30,30,35,35,35,35,35,40,40,40,40,40,45,45,45,45,45,50,50,50,50,50,55,55,55,55,55,60,60,60,60]

-- Note: 'level 0' will return a negative shift, don't use it
level :: Key -> Int
level w = 60 - l + mod l 5 -- (countLeadingZeros w .&. 0x7c)
  where l = countLeadingZeros w
{-# INLINE level #-}

maskBit :: Key -> Offset -> Int
maskBit k o = fromIntegral (unsafeShiftR k o .&. 0x1f)
{-# INLINE maskBit #-}

mask :: Key -> Offset -> Mask
mask k o = unsafeShiftL 1 (maskBit k o)
{-# INLINE mask #-}

offset :: Int -> Mask -> Int
offset k w = popCount $ w .&. (unsafeShiftL 1 k - 1)
{-# INLINE offset #-}

insert :: Key -> v -> WordMap v -> WordMap v
insert !k v xs0 = go xs0 where
  pair o ok on
    | k < ok    = Fork k ok o (Tip k v) on
    | otherwise = Fork ok k o on (Tip k v)
  go on@(Full ok n as)
    | wd > 0x1f = pair (level okk) ok on
    | !oz <- indexSmallArray as d
    , !z <- go oz
    , ptrNeq z oz = Full ok n (update32 d z as)
    | otherwise = on
    where
      okk = xor ok k
      wd  = unsafeShiftR okk n
      d   = fromIntegral wd
  go on@(Node ok n m as)
    | wd > 0x1f = pair (level okk) ok on
    | m .&. b == 0 = node ok n (m .|. b) (insertSmallArray odm (Tip k v) as)
    | !oz <- indexSmallArray as odm
    , !z <- go oz
    , ptrNeq z oz = Node ok n m (updateSmallArray odm z as)
    | otherwise = on
    where
      okk = xor ok k
      wd  = unsafeShiftR okk n
      d   = fromIntegral wd
      b   = unsafeShiftL 1 d
      odm = popCount $ m .&. (b - 1)
  go on@(Tip ok ov)
    | k /= ok    = pair (level (xor ok k)) ok on
    | ptrEq v ov = on
    | otherwise  = Tip k v
  go on@(Fork lk rk o l r)
    | wdl > 0x1f = pair (level klk) lk on
    | wdl == 0, !l' <- go l = if ptrEq l l' then on else Fork lk rk o l' r
    | wdr == 0, !r' <- go r = if ptrEq r r' then on else Fork lk rk o l r'
    | otherwise = Node (k .&. unsafeShiftL 0xffffffffffffffe0 o) o (mask lk o .|. mask rk o .|. mask k o) $ runST $ do
      arr <- newSmallArray 3 (Tip k v)
      writeSmallArray arr (fromEnum (k < lk)) l
      writeSmallArray arr (1 + fromEnum (k < rk)) r
      unsafeFreezeSmallArray arr
    where klk = xor k lk
          wdl = unsafeShiftR klk o
          krk = xor k rk
          wdr = unsafeShiftR krk o
  go Nil = Tip k v
{-# INLINEABLE insert #-}

lookup :: Key -> WordMap v -> Maybe v
lookup !k (Full ok o a)
  | z <- unsafeShiftR (xor k ok) o, z <= 0x1f = lookup k $ indexSmallArray a (fromIntegral z)
  | otherwise = Nothing
lookup k (Node ok o m a)
  | z <= 0x1f && m .&. b /= 0 = lookup k (indexSmallArray a (popCount (m .&. (b - 1))))
  | otherwise = Nothing
  where
    z = unsafeShiftR (xor k ok) o
    b = unsafeShiftL 1 (fromIntegral z)
lookup k (Fork lk rk o l r)
  | unsafeShiftR (xor k lk) o == 0 = lookup k l
  | unsafeShiftR (xor k rk) o == 0 = lookup k r
  | otherwise = Nothing
lookup k (Tip ok ov)
  | k == ok   = Just ov
  | otherwise = Nothing
lookup _ Nil = Nothing
{-# INLINEABLE lookup #-}

member :: Key -> WordMap v -> Bool
member !k (Full ok o a)
  | z <- unsafeShiftR (xor k ok) o = z <= 0x1f && member k (indexSmallArray a (fromIntegral z))
member k (Node ok o m a)
  | z <- unsafeShiftR (xor k ok) o
  = z <= 0x1f && let b = unsafeShiftL 1 (fromIntegral z) in
    m .&. b /= 0 && member k (indexSmallArray a (popCount (m .&. (b - 1))))
member k (Fork lk rk o l r)
  =  (unsafeShiftR (xor k lk) o == 0 && member k l)
  || (unsafeShiftR (xor k rk) o == 0 && member k r)
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

update32 :: Int -> a -> SmallArray a -> SmallArray a
update32 !k a i = runST $ do
  o <- clone32 i
  writeSmallArray o k a
  unsafeFreezeSmallArray o
{-# INLINEABLE update32 #-}

insertSmallArray :: Int -> a -> SmallArray a -> SmallArray a
insertSmallArray !k a i = runST $ do
  let n = length i
  o <- newSmallArray (n + 1) a
  copySmallArray  o 0 i 0 k
  copySmallArray  o (k+1) i k (n-k)
  unsafeFreezeSmallArray o
{-# INLINEABLE insertSmallArray #-}

clone32 :: SmallArray a -> ST s (SmallMutableArray s a)
clone32 i = do
  o <- newSmallArray 32 undefined
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
  indexSmallArrayM i 16 >>= writeSmallArray o 16
  indexSmallArrayM i 17 >>= writeSmallArray o 17
  indexSmallArrayM i 18 >>= writeSmallArray o 18
  indexSmallArrayM i 19 >>= writeSmallArray o 19
  indexSmallArrayM i 20 >>= writeSmallArray o 20
  indexSmallArrayM i 21 >>= writeSmallArray o 21
  indexSmallArrayM i 22 >>= writeSmallArray o 22
  indexSmallArrayM i 23 >>= writeSmallArray o 23
  indexSmallArrayM i 24 >>= writeSmallArray o 24
  indexSmallArrayM i 25 >>= writeSmallArray o 25
  indexSmallArrayM i 26 >>= writeSmallArray o 26
  indexSmallArrayM i 27 >>= writeSmallArray o 27
  indexSmallArrayM i 28 >>= writeSmallArray o 28
  indexSmallArrayM i 29 >>= writeSmallArray o 29
  indexSmallArrayM i 30 >>= writeSmallArray o 30
  indexSmallArrayM i 31 >>= writeSmallArray o 31
  return o
{-# INLINE clone32 #-}

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
