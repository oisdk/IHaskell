{-# LANGUAGE ScopedTypeVariables #-}

module Data.Tree.KD where

import           Data.Vector.Algorithms.Intro
import qualified Data.Vector.Generic          as Vector
import qualified Data.Vector.Generic.Mutable  as MVector
import           Control.Monad.ST

import Data.List.NonEmpty (NonEmpty(..))
import Data.Stream


data Tree v a
  = Tree
  { accessors :: NonEmpty (a -> a -> Ordering)
  , storage   :: v a
  }

buildTree
    :: (Vector.Vector v a)
    => NonEmpty (a -> a -> Ordering) -> v a -> Tree v a
buildTree cmps (xs :: v a) = undefined --Tree cmps (go (toStream cmps) xs)
  -- where
  --   go (c :< cs) xs = Vector.parition

member
    :: Vector.Vector v a
    => a -> Tree v a -> Bool
member x (Tree ks xs) = go (toStream ks) 0 (Vector.length xs)
  where
    go (c :< cs) l u
      | l >= u = False
      | otherwise =
          case c x (xs Vector.! k) of
              LT -> go cs l k
              EQ -> case foldMap (\y -> y x (xs Vector.! k)) ks of
                EQ -> True
                _ -> go cs l k || go cs (k + 1) u
              GT -> go cs (k + 1) u
      where
        k = l + ((u - l) `div` 2)

