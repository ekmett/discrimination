{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# OPTIONS_GHC -funbox-strict-fields -O2 #-}
module Main where

import Control.Applicative
import Control.DeepSeq
import Control.Exception (evaluate)
import Control.Monad.ST
import Criterion.Main
import Data.Bits
import Data.Bits.Extras
import Data.Foldable
import Data.Functor
import Data.HashMap.Strict (HashMap)
import Data.Maybe (fromMaybe)
import Data.Monoid
import Data.Primitive.Array
import Data.Traversable
import Data.Word
import qualified GHC.Exts as Exts
import Prelude hiding (lookup, length, foldr)
import qualified Prelude
import qualified Data.IntMap as M
import qualified Data.HashMap.Strict as H
import GHC.Types

type Key = Word64
type Mask = Word16
type Offset = Int

pattern N = 16

length :: Array a -> Int
length (Array ary) = I# (Exts.sizeofArray# ary)
{-# INLINE length #-}

instance Exts.IsList (Array a) where
  type Item (Array a) = a
  toList !arr = go 0 where
    n = length arr
    go !k
      | k == n    = []
      | otherwise = indexArray arr k : go (k+1)
  fromListN n xs0 = runST $ do
    arr <- newArray n undefined
    let go _ []     = return ()
        go k (x:xs) = writeArray arr k x >> go (k+1) xs
    go 0 xs0
    unsafeFreezeArray arr
  fromList xs = Exts.fromListN (Prelude.length xs) xs

instance Functor Array where
  fmap f !i = runST $ do
    let n = length i
    o <- newArray n undefined
    let go k 
          | k == n = return ()
          | otherwise = do
            a <- indexArrayM i k 
            writeArray o k (f a)
            go (k+1)
    go 0
    unsafeFreezeArray o

instance Foldable Array where
  foldr f z a = foldr f z (Exts.toList a)
  foldMap f a = foldMap f (Exts.toList a)

instance Traversable Array where
  traverse f a = Exts.fromListN (length a) <$> traverse f (Exts.toList a)
  
data WordMap v
  = Nil
  | Tip  !Key v
  | Node !Key !Offset !Mask !(Array (WordMap v))
  | Full !Key !Offset !(Array (WordMap v))

node :: Key -> Offset -> Mask -> Array (WordMap v) -> WordMap v
node k o 0xffff a = Full k o a
node k o m a      = Node k o m a

instance NFData v => NFData (WordMap v) where
  rnf Nil = ()
  rnf (Tip _ v) = rnf v
  rnf (Node _ _ _ a) = rnf (Exts.toList a)
  rnf (Full _ _ a) = rnf (Exts.toList a)

instance Show v => Show (WordMap v) where
  showsPrec _ Nil = showString "Nil"
  showsPrec d (Tip k v) = showParen (d > 10) $
     showString "Tip " . showsPrec 11 k . showChar ' ' . showsPrec 11 v
  showsPrec d (Node k o m as) = showParen (d > 10) $
     showString "Node " . showsPrec 11 k . showChar ' ' . showsPrec 11 o . showChar ' ' . showsPrec 11 m . showChar ' ' . showParen True 
     (showString "fromListN " . showsPrec 11 (length as) . showChar ' ' . showsPrec 11 (Exts.toList as))
  showsPrec d (Full k o as) = showParen (d > 10) $
     showString "Full " . showsPrec 11 k . showChar ' ' . showsPrec 11 o . showChar ' ' . showParen True 
     (showString "fromListN " . showsPrec 11 (length as) . showChar ' ' . showsPrec 11 (Exts.toList as))

instance Functor WordMap where
  fmap f = go where
    go Nil = Nil
    go (Tip k v) = Tip k (f v)
    go (Node k o m a) = Node k o m (fmap go a)
    go (Full k o a) = Full k o (fmap go a)
  {-# INLINE fmap #-}

instance Foldable WordMap where
  foldMap f = go where
    go Nil = mempty
    go (Tip _ v) = f v
    go (Node _ _ _ a) = foldMap go a
    go (Full _ _ a) = foldMap go a
  {-# INLINE foldMap #-}

instance Traversable WordMap where
  traverse f = go where
    go Nil = pure Nil
    go (Tip k v) = Tip k <$> f v
    go (Node k o m a) = Node k o m <$> traverse go a
    go (Full k o a) = Full k o <$> traverse go a
  {-# INLINE traverse #-}

-- Note: 'level 0' will return a negative shift, don't use it
level :: Key -> Int
level w = 60 - (nlz w .&. 0x7c)
{-# INLINE level #-}

maskBit :: Key -> Offset -> Int
maskBit k o = fromIntegral (unsafeShiftR k o .&. 0xf)
{-# INLINE maskBit #-}

mask :: Key -> Offset -> Word16
mask k o = bit (maskBit k o)
{-# INLINE mask #-}

offset :: Int -> Word16 -> Int
offset k w = popCount $ w .&. (bit k - 1)
{-# INLINE offset #-}

insert :: Key -> v -> WordMap v -> WordMap v
insert !k v xs0 = go xs0 where 
  go Nil = Tip k v
  go ot@(Tip ok _)
    | k == ok = t
    | o <- level (xor k ok) = Node k o (setBit (mask k o) (maskBit ok o))
         $ if k < ok then two t ot else two ot t
    where t = Tip k v
  go on@(Node ok n m as)
    | o <- level (xor k ok)
    = if
      | o > n -> Node k o (setBit (mask k o) (maskBit ok o))
               $ if k < ok then two (Tip k v) on else two on (Tip k v)
      | d <- maskBit k n -> if 
        | testBit m d -> Node ok n m            (updateArray (offset d m) go as)
        | otherwise   -> node ok n (setBit m d) (insertArray (offset d m) (Tip k v) as)
  go on@(Full ok n as)
    | o <- level (xor k ok)
    = if 
      | o > n -> Node k o (setBit (mask k o) (maskBit ok o))
               $ if k < ok then two (Tip k v) on else two on (Tip k v)
      | otherwise -> Full ok n (updateArray (maskBit k n) go as)
      
{-# INLINE insert #-}

lookup :: Key -> WordMap v -> Maybe v
lookup !k xs0 = go xs0 where
  go Nil = Nothing
  go (Tip ok ov)
    | k == ok   = Just ov
    | otherwise = Nothing
  go (Node _ o m a)
    | d <- maskBit k o, testBit m d = go $ indexArray a (offset d m)
    | otherwise = Nothing
  go (Full _ o a) = go (indexArray a (maskBit k o))
{-# INLINE lookup #-}

member :: Key -> WordMap v -> Bool
member !k xs0 = go xs0 where
  go Nil = False
  go (Tip ok _) = k == ok
  go (Node _ o m a) | d <- maskBit k o = testBit m d && go (indexArray a (offset d m))
  go (Full _ o a) = go (indexArray a (maskBit k o))
{-# INLINE member #-}
  
updateArray :: Int -> (a -> a) -> Array a -> Array a
updateArray !k f i = runST $ do
  let n = length i
  o <- newArray n undefined
  copyArray o 0 i 0 n
  a <- indexArrayM i k 
  writeArray o k (f a)
  unsafeFreezeArray o

insertArray :: Int -> a -> Array a -> Array a
insertArray !k a i = runST $ do
  let n = length i
  o <- newArray (n + 1) undefined
  copyArray  o 0 i 0 k 
  writeArray o k a
  copyArray  o (k+1) i k (n-k)
  unsafeFreezeArray o

two :: a -> a -> Array a
two a b = runST $ do
  arr <- newArray 2 b
  writeArray arr 0 a
  unsafeFreezeArray arr

-- so we need to be able to quickly check if we differ on a higher nybble than the one
-- the word32s are the least and greatest keys possible in this node, not present

singleton :: Key -> v -> WordMap v
singleton !k v = Tip k v

fromList :: [(Word64,v)] -> WordMap v
fromList xs = foldl' (\r (k,v) -> insert k v r) Nil xs

main = do
    let denseM = M.fromAscList elems :: M.IntMap Int
        denseW = fromList welems :: WordMap Word64
        denseH = H.fromList welems :: HashMap Word64 Word64
        sparseM = M.fromAscList sElems :: M.IntMap Int
        sparseW = fromList wsElems :: WordMap Word64
        sparseH = H.fromList wsElems :: HashMap Word64 Word64
        sparseM' = M.fromAscList sElemsSearch :: M.IntMap Int
        sparseW' = fromList wsElemsSearch :: WordMap Word64
        sparseH' = H.fromList wsElemsSearch :: HashMap Word64 Word64
    evaluate $ rnf [denseM, sparseM, sparseM']
    evaluate $ rnf [denseW, sparseW, sparseW']
    evaluate $ rnf [denseH, sparseH, sparseH']
    evaluate $ rnf [elems,  sElems,  sElemsSearch]
    evaluate $ rnf [keys,   sKeys, sKeysSearch]
    evaluate $ rnf [values, sValues]
    evaluate $ rnf [welems,  wsElems,  wsElemsSearch]
    evaluate $ rnf [wkeys,   wsKeys, wsKeysSearch]
    evaluate $ rnf [wvalues, wsValues]
    defaultMain
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
        , bgroup "insert"
            [ bgroup "present"
                [ bench "IntMap"  $ whnf (\m -> foldl' (\m (k, v) -> M.insert k v m) m elems) denseM
                , bench "WordMap" $ whnf (\m -> foldl' (\m (k, v) -> insert k v m) m welems) denseW
                , bench "HashMap" $ whnf (\m -> foldl' (\m (k, v) -> H.insert k v m) m welems) denseH
                ]
            , bgroup "absent"
                [ bench "IntMap" $ whnf (\m -> foldl' (\m (k, v) -> M.insert k v m) m sElemsSearch) sparseM
                , bench "WordMap" $ whnf (\m -> foldl' (\m (k, v) -> insert k v m) m wsElemsSearch) sparseW
                , bench "HashMap" $ whnf (\m -> foldl' (\m (k, v) -> H.insert k v m) m wsElemsSearch) sparseH
                ]
            ]
        ]
  where
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
