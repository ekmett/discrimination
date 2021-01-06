{-# LANGUAGE ScopedTypeVariables #-}
module Main (main) where

import Data.Complex (Complex)
import Data.Int (Int8, Int16, Int32, Int64)
import Data.Proxy (Proxy (..))
import Data.Typeable (Typeable, typeRep)
import Data.Word (Word8, Word16, Word32, Word64)
import Numeric.Natural (Natural)
import Test.QuickCheck (Arbitrary, Property, label, (===))
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
    -- , testGrouping (Proxy :: Proxy Natural) -- broken!
    -- , testGrouping (Proxy :: Proxy Integer) -- broken!
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
    ]
  ]

testGrouping
  :: forall a. (Grouping a, Typeable a, Arbitrary a, Eq a, Show a)
  => Proxy a
  -> TestTree
testGrouping p = testProperty name prop
  where
    name = show (typeRep p)

    prop :: a -> a -> Property
    prop x y = label (show lhs) $ lhs === groupingEq x y
      where
        lhs = x == y

testSorting
  :: forall a. (Sorting a, Typeable a, Arbitrary a, Ord a, Show a)
  => Proxy a
  -> TestTree
testSorting p = testProperty name prop
  where
    name = show (typeRep p)

    prop :: a -> a -> Property
    prop x y = label (show lhs) $ lhs === sortingCompare x y
      where
        lhs = compare x y
