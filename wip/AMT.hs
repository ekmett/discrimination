{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE MultiWayIf #-}
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
import Data.Foldable hiding (foldl')
import Data.Functor
import Data.List (foldl')
import Data.Maybe (fromMaybe)
import Data.Monoid
import Data.Primitive.Array
import Data.Traversable
import Data.Word
import Prelude hiding (lookup)
import qualified Data.IntMap as M

type Key = Word64
type Mask = Word16
type Offset = Int

pattern N = 16

fromArray :: Int -> Array a -> [a]
fromArray !n !arr = go 0 where
  go !k
    | k == n    = []
    | otherwise = indexArray arr k : go (k+1)

toArray :: Int -> [a] -> Array a
toArray n xs0 = runST $ do
  arr <- newArray n undefined
  let go k []     = return ()
      go k (x:xs) = writeArray arr k x >> go (k+1) xs
  go 0 xs0
  unsafeFreezeArray arr

-- Assumes n >= 1
mapArray :: Int -> (a -> b) -> Array a -> Array b
mapArray !n f !i = runST $ do
  o <- newArray n undefined
  let go k 
        | k == n = return ()
        | otherwise = do
          a <- indexArrayM i k 
          writeArray o k (f a)
          go (k+1)
  go 0
  unsafeFreezeArray o

data WordMap v
  = Nil
  | Tip  !Key v
  | Node !Key !Offset !Mask !(Array (WordMap v))

instance NFData v => NFData (WordMap v) where
  rnf Nil = ()
  rnf (Tip _ v) = rnf v
  rnf (Node _ _ m a) = rnf (fromArray (popCount m) a)

instance Show v => Show (WordMap v) where
  showsPrec _ Nil = showString "Nil"
  showsPrec d (Tip k v) = showParen (d > 10) $
     showString "Tip " . showsPrec 11 k . showChar ' ' . showsPrec 11 v
  showsPrec d (Node k o m as) = showParen (d > 10) $
     showString "Node " . showsPrec 11 k . showChar ' ' . showsPrec 11 o . showChar ' ' . showsPrec 11 m . showChar ' ' . showParen True 
     (showString "toArray " . showsPrec 11 n . showChar ' ' . showsPrec 11 (fromArray n as))
     where n = popCount m

instance Functor WordMap where
  fmap f = go where
    go Nil = Nil
    go (Tip k v) = Tip k (f v)
    go (Node k o m a) = Node k o m (mapArray (popCount m) go a)

instance Foldable WordMap where
  foldMap f = go where
    go Nil = mempty
    go (Tip _ v) = f v
    go (Node _ _ m a) = foldMap go (fromArray (popCount m) a)

instance Traversable WordMap where
  traverse f = go where
    go Nil = pure Nil
    go (Tip k v) = Tip k <$> f v
    go (Node k o m a) = Node k o m . toArray n <$> traverse go (fromArray n a) where n = popCount m 

-- Note: 'level 0' will return a negative shift, don't use it
level :: Key -> Int
level w = 60 - (nlz w .&. 0xfc)

maskBit :: Key -> Offset -> Int
maskBit k o = fromIntegral (unsafeShiftR k o .&. 0xf)

mask :: Key -> Offset -> Word16
mask k o = bit (maskBit k o)

offset :: Int -> Word16 -> Int
offset k w = popCount $ w .&. (bit k - 1)

insert :: Key -> v -> WordMap v -> WordMap v
insert k v Nil = Tip k v
insert k v ot@(Tip ok ov)
  | k == ok = t
  | o <- level (xor k ok) = Node k o (setBit (mask k o) (maskBit ok o))
       $ if k < ok then two t ot else two ot t
  where t = Tip k v
insert k v on@(Node ok n m as)
  | o <- level (xor k ok)
  = if
    | o > n -> Node k o (setBit (mask k o) (maskBit ok o))
             $ if k < ok then two (Tip k v) on else two on (Tip k v)
    | d <- maskBit k n -> if 
      | testBit m d -> Node ok n m            (updateArray (offset d m) (popCount m) (insert k v) as)
      | otherwise   -> Node ok n (setBit m d) (insertArray (offset d m) (popCount m) (Tip k v) as)

lookup :: Key -> WordMap v -> Maybe v
lookup _ Nil = Nothing
lookup k (Tip ok ov)
  | k == ok   = Just ov
  | otherwise = Nothing
lookup k (Node ok o m a) 
  | d <- maskBit k o, testBit m d = lookup k $ indexArray a (offset d m)
  | otherwise = Nothing

member :: Key -> WordMap v -> Bool
member _ Nil = False
member k (Tip ok _) = k == ok
member k (Node ok o m a) | d <- maskBit k o = testBit m d && member k (indexArray a (offset d m))
  
updateArray :: Int -> Int -> (a -> a) -> Array a -> Array a
updateArray k n f i = runST $ do
  o <- newArray n undefined
  copyArray o 0 i 0 n
  a <- indexArrayM i k 
  writeArray o k (f a)
  unsafeFreezeArray o

insertArray :: Int -> Int -> a -> Array a -> Array a
insertArray k n a i = runST $ do
  o <- newArray (n + 1) undefined
  copyArray  o 0 i 0 k 
  writeArray o k a
  copyArray  o (k+1) i k (n-k)
  unsafeFreezeArray o

two :: a -> a -> Array a
two a b = runST $ do
  arr <- newArray 2 a
  writeArray arr 1 b
  unsafeFreezeArray arr

-- so we need to be able to quickly check if we differ on a higher nybble than the one
-- the word32s are the least and greatest keys possible in this node, not present

singleton :: Key -> v -> WordMap v
singleton k v = Tip k v

fromList :: [(Word64,v)] -> WordMap v
fromList = foldl' (\r (k,v) -> insert k v r) Nil 

main = do
    let denseM = M.fromAscList elems :: M.IntMap Int
        denseW = fromList welems :: WordMap Word64
        sparseM = M.fromAscList sElems :: M.IntMap Int
        sparseW = fromList wsElems :: WordMap Word64
        sparseM' = M.fromAscList sElemsSearch :: M.IntMap Int
        sparseW' = fromList wsElemsSearch :: WordMap Word64
    evaluate $ rnf [denseM, sparseM, sparseM']
    evaluate $ rnf [denseW, sparseW, sparseW']
    evaluate $ rnf [elems,  sElems,  sElemsSearch]
    evaluate $ rnf [keys,   sKeys,   sKeysSearch]
    evaluate $ rnf [values, sValues]
    evaluate $ rnf [welems,  wsElems,  wsElemsSearch]
    evaluate $ rnf [wkeys,   wsKeys,   wsKeysSearch]
    evaluate $ rnf [wvalues, wsValues]
    defaultMain
        [ bgroup "lookup"
            [ bgroup "present"
                [ bench "IntMap"  $ whnf (\m -> foldl' (\n k -> fromMaybe n (M.lookup k m)) 0 keys) denseM
                , bench "WordMap" $ whnf (\m -> foldl' (\n k -> fromMaybe n (lookup k m)) 0 wkeys) denseW
                ]
            , bgroup "absent"
                [ bench "IntMap"  $ whnf (\m -> foldl' (\n k -> fromMaybe n (M.lookup k m)) 0 sKeysSearch) sparseM
                , bench "WordMap" $ whnf (\m -> foldl' (\n k -> fromMaybe n (lookup k m)) 0 wsKeysSearch) sparseW
                ]
            ]
        , bgroup "member"
            [ bgroup "present"
                [ bench "IntMap"  $ whnf (\m -> foldl' (\n x -> if M.member x m then n + 1 else n) 0 keys) denseM
                , bench "WordMap" $ whnf (\m -> foldl' (\n x -> if member x m then n + 1 else n) 0 wkeys) denseW
                ]
            , bgroup "absent"
                [ bench "IntMap"  $ whnf (\m -> foldl' (\n x -> if M.member x m then n + 1 else n) 0 sKeysSearch) sparseM
                , bench "WordMap" $ whnf (\m -> foldl' (\n x -> if member x m then n + 1 else n) 0 wsKeysSearch) sparseW
                ]
            ]
        , bgroup "insert"
            [ bgroup "present"
                [ bench "IntMap"  $ whnf (\m -> foldl' (\m (k, v) -> M.insert k v m) m elems) denseM
                , bench "WordMap" $ whnf (\m -> foldl' (\m (k, v) -> insert k v m) m welems) denseW
                ]
            , bgroup "absent"
                [ bench "IntMap" $ whnf (\m -> foldl' (\m (k, v) -> M.insert k v m) m sElemsSearch) sparseM
                , bench "WordMap" $ whnf (\m -> foldl' (\m (k, v) -> insert k v m) m wsElemsSearch) sparseW
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
