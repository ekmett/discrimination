{-# LANGUAGE ScopedTypeVariables #-}
module Main (main) where

import Data.Complex (Complex)
import Data.Int (Int8, Int16, Int32, Int64)
import Data.List.NonEmpty (NonEmpty)
import Data.Proxy (Proxy (..))
import Data.Typeable (Typeable, typeRep)
import Data.Word (Word8, Word16, Word32, Word64)
import Numeric.Natural (Natural)
import Test.QuickCheck (Arbitrary (..), Property, counterexample, label, (===),
                        sized, chooseInt, vectorOf)
import Test.QuickCheck.Instances ()
import Test.Tasty (defaultMain, testGroup, TestTree)
import Test.Tasty.QuickCheck (testProperty)

import qualified Data.List as L

import Data.Discrimination
import Utils

main :: IO ()
main = defaultMain $ testGroup "discrimination"
  [ testGroup "examples"
    [ testGroup "nub"
      [ testProperty "List.nub" $
        let prop :: [Word64] -> Property
            prop xs = L.nub xs === nub xs
        in prop

      , testProperty "ordNub" $
        let prop :: [Word64] -> Property
            prop xs = ordNub xs === nub xs
        in prop

      , testProperty "hashNub" $
        let prop :: [Word64] -> Property
            prop xs = hashNub xs === nub xs
        in prop
      ]

    , testGroup "sort"
      [ testProperty "List.sort" $
        let prop :: [Word64] -> Property
            prop xs = L.sort xs === sort xs
        in prop

      , testProperty "introsort" $
        -- for Word64 unstable sort works too
        let prop :: [Word64] -> Property
            prop xs = introsort xs === sort xs
        in prop

      , testProperty "mergesort" $
        let prop :: [Word64] -> Property
            prop xs = mergesort xs === sort xs
        in prop
      ]
    ]

  , testGroup "Grouping"
    [ testGrouping (Proxy :: Proxy ())
    , testGrouping (Proxy :: Proxy Int)
    , testGrouping (Proxy :: Proxy Int8)
    , testGrouping (Proxy :: Proxy Int16)
    , testGrouping (Proxy :: Proxy Int32)
    , testGrouping (Proxy :: Proxy Int64)
    , testGrouping (Proxy :: Proxy Word)
    , testGrouping (Proxy :: Proxy Word8)
    , testGrouping (Proxy :: Proxy Word16)
    , testGrouping (Proxy :: Proxy Word32)
    , testGrouping (Proxy :: Proxy Word64)
    , testGrouping (Proxy :: Proxy Bool)
    , testGrouping (Proxy :: Proxy Ordering)
    , testGrouping (Proxy :: Proxy (Word8,Word8))
    , testGrouping (Proxy :: Proxy (Word8,Word8,Word8))
    , testGrouping (Proxy :: Proxy (Word8,Word8,Word8,Word8))
    , testGrouping (Proxy :: Proxy (Complex Word8))
    , testGrouping (Proxy :: Proxy (Maybe Word8))
    , testGrouping (Proxy :: Proxy (Either Word8 Word8))
    , testGrouping (Proxy :: Proxy Char)
    , testGrouping (Proxy :: Proxy String)
    , testGrouping (Proxy :: Proxy (NonEmpty Int))
    , testGrouping (Proxy :: Proxy Natural)
    , testGrouping (Proxy :: Proxy Integer)

    , testGrouping' listToNatural
    , testGrouping' listToInteger
    ]

  , testGroup "Sorting"
    [ testSorting (Proxy :: Proxy ())
    , testSorting (Proxy :: Proxy Int)
    , testSorting (Proxy :: Proxy Int8)
    , testSorting (Proxy :: Proxy Int16)
    , testSorting (Proxy :: Proxy Int32)
    , testSorting (Proxy :: Proxy Int64)
    , testSorting (Proxy :: Proxy Word)
    , testSorting (Proxy :: Proxy Word8)
    , testSorting (Proxy :: Proxy Word16)
    , testSorting (Proxy :: Proxy Word32)
    , testSorting (Proxy :: Proxy Word64)
    , testSorting (Proxy :: Proxy Bool)
    , testSorting (Proxy :: Proxy Ordering)
    , testSorting (Proxy :: Proxy (Word8,Word8))
    , testSorting (Proxy :: Proxy (Word8,Word8,Word8))
    , testSorting (Proxy :: Proxy (Word8,Word8,Word8,Word8))
    , testSorting (Proxy :: Proxy (Maybe Word8))
    , testSorting (Proxy :: Proxy (Either Word8 Word8))
    , testSorting (Proxy :: Proxy Char)
    , testSorting (Proxy :: Proxy String)
    , testSorting (Proxy :: Proxy (NonEmpty Int))
    , testSorting (Proxy :: Proxy Natural)
    , testSorting (Proxy :: Proxy Integer)

    , testSorting' listToNatural
    , testSorting' listToInteger
    ]
  ]

listToNatural :: SmallList Word64 -> Natural
listToNatural = L.foldl' (\x y -> x * 2 ^ (64 :: Int) + fromIntegral y) 0 . getSmallList

listToInteger :: SmallList Int64 -> Integer
listToInteger = L.foldl' (\x y -> x * 2 ^ (64 :: Int) + fromIntegral y) 0 . getSmallList

newtype SmallList a = SmallList { getSmallList :: [a] } deriving (Eq, Show)

instance Arbitrary a => Arbitrary (SmallList a) where
    arbitrary = sized $ \n -> do
        m <- chooseInt (0, min 10 n)
        SmallList <$> vectorOf m arbitrary

    shrink = fmap SmallList . shrink . getSmallList

testGrouping
  :: forall a. (Grouping a, Typeable a, Arbitrary a, Eq a, Show a)
  => Proxy a
  -> TestTree
testGrouping _ = testGrouping' (id :: a -> a)

testGrouping'
  :: forall a b. (Grouping b, Typeable a, Typeable b, Arbitrary a, Eq b, Show a, Show b)
  => (a -> b)
  -> TestTree
testGrouping' f = testGroup name
    [ testProperty "groupingEq" prop_eq
    , testProperty "nub"        prop_nub
    ]
  where
    tra = typeRep (Proxy :: Proxy a)
    trb = typeRep (Proxy :: Proxy b)
    name = if tra == trb then show tra else show trb ++ " from " ++ show tra

    prop_eq :: a -> a -> Property
    prop_eq x' y' =
        counterexample (show (x,y)) $
        label (show lhs) $
        lhs === groupingEq x y
      where
        x = f x'
        y = f y'
        lhs = x == y

    prop_nub :: [a] -> Property
    prop_nub xs' = L.nub xs === nub xs
      where
        xs = take 100 (map f xs')

testSorting
  :: forall a. (Sorting a, Typeable a, Arbitrary a, Ord a, Show a)
  => Proxy a
  -> TestTree
testSorting _ = testSorting' (id :: a -> a)

testSorting'
  :: forall a b. (Sorting b, Typeable a, Typeable b, Arbitrary a, Ord b, Show a, Show b)
  => (a -> b)
  -> TestTree
testSorting' f = testGroup name
    [ testProperty "sortingCompare" prop_cmp
    , testProperty "sort"           prop_sort
    ]
  where
    tra = typeRep (Proxy :: Proxy a)
    trb = typeRep (Proxy :: Proxy b)
    name = if tra == trb then show tra else show trb ++ " from " ++ show tra

    prop_cmp :: a -> a -> Property
    prop_cmp x' y' =
        counterexample (show (x,y)) $
        label (show lhs) $
        lhs === sortingCompare x y
      where
        x = f x'
        y = f y'
        lhs = compare x y

    prop_sort :: [a] -> Property
    prop_sort xs' = L.sort xs === sort xs
      where
        xs = map f xs'
