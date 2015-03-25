{-# LANGUAGE CPP #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Data.Discrimination.Join
 ( joining, inner, outer, leftOuter, rightOuter
 ) where

import Control.Applicative
import Control.Arrow
import Data.Maybe (catMaybes)
import Data.Discrimination.Type

-- | Optimized and CPS'd version of 'Data.Either.partitionEithers', where all lefts are known to come before all rights
spanEither :: ([a] -> [b] -> c) -> [Either a b] -> c
spanEither k xs0 = go [] xs0 where
  go acc (Left x:xs) = go (x:acc) xs
  go acc rights = k (reverse acc) (map fromRight rights)
  fromRight (Right y) = y
  fromRight _ = error "spanEither: unstable"

joining
  :: Disc d
  -> ([a] -> [b] -> c)
  -> (a -> d)
  -> (b -> d)
  -> [a]
  -> [b]
  -> [c]
joining disc abc ad bd as bs = spanEither abc <$> runDisc disc (map (ad &&& Left) as ++ map (bd &&& Right) bs)
{-# INLINE joining #-}

inner
  :: Disc d
  -> (a -> b -> c)
  -> (a -> d)
  -> (b -> d)
  -> [a]
  -> [b]
  -> [[c]]
inner disc abc ad bd as bs = catMaybes $ joining disc go ad bd as bs where
  go ap bp
    | null ap || null bp = Nothing
    | otherwise = Just (liftA2 abc ap bp)

outer
  :: Disc d
  -> (a -> b -> c)
  -> (a -> c)
  -> (b -> c)
  -> (a -> d)
  -> (b -> d)
  -> [a]
  -> [b]
  -> [[c]]
outer disc abc ac bc ad bd as bs = joining disc go ad bd as bs where
  go ap bp
    | null ap = map bc bp
    | null bp = map ac ap
    | otherwise = liftA2 abc ap bp

leftOuter
  :: Disc d
  -> (a -> b -> c)
  -> (a -> c)
  -> (a -> d)
  -> (b -> d)
  -> [a]
  -> [b]
  -> [[c]]
leftOuter disc abc ac ad bd as bs = catMaybes $ joining disc go ad bd as bs where
  go ap bp
    | null ap = Nothing
    | null bp = Just (map ac ap)
    | otherwise = Just (liftA2 abc ap bp)

rightOuter
  :: Disc d
  -> (a -> b -> c)
  -> (b -> c)
  -> (a -> d)
  -> (b -> d)
  -> [a]
  -> [b]
  -> [[c]]
rightOuter disc abc bc ad bd as bs = catMaybes $ joining disc go ad bd as bs where
  go ap bp
    | null bp = Nothing
    | null ap = Just (map bc bp)
    | otherwise = Just (liftA2 abc ap bp)
