module Main where

import Control.Exception (evaluate)
import Control.DeepSeq (rnf)
import Criterion.Main
import Criterion.Types

import qualified Data.List as L
import qualified System.Random.SplitMix as SM

import Utils

import Data.Discrimination (nub, sort)


main :: IO ()
main = do
  let gen = SM.mkSMGen 0xc0de

  let xs100   = finiteList gen 100
  let xs1000  = finiteList gen 1000
  let xs10000 = finiteList gen 10000

  evaluate $ rnf xs100
  evaluate $ rnf xs1000
  evaluate $ rnf xs10000

  defaultMainWith (defaultConfig { timeLimit = 1 })
    [ bgroup "nub"
      [ bgroup "List.nub"
        [ bench "100" $ whnf (length . L.nub) xs100
        , bench "1000" $ whnf (length . L.nub) xs1000
        ]
      , bgroup "ordNub"
        [ bench "100" $ whnf (length . ordNub) xs100
        , bench "1000" $ whnf (length . ordNub) xs1000
        , bench "10000" $ whnf (length . ordNub) xs10000
        ]
      , bgroup "hashNub"
        [ bench "100" $ whnf (length . hashNub) xs100
        , bench "1000" $ whnf (length . hashNub) xs1000
        , bench "10000" $ whnf (length . hashNub) xs10000
        ]
      , bgroup "Discrimination.nub"
        [ bench "100" $ whnf (length . nub) xs100
        , bench "1000" $ whnf (length . nub) xs1000
        , bench "10000" $ whnf (length . nub) xs10000
        ]
      ]

    , bgroup "sort"
      [ bgroup "List.sort"
        [ bench "10000" $ whnf (length . L.sort) xs10000
        ]
      , bgroup "mergesort"
        [ bench "10000" $ whnf (length . mergesort) xs10000
        ]
      , bgroup "introsort"
        [ bench "10000" $ whnf (length . introsort) xs10000
        ]
      , bgroup "Discrimination.sort"
        [ bench "10000" $ whnf (length . sort) xs10000
        ]
      ]
    ]
