{-# LANGUAGE ScopedTypeVariables #-}

module Data.Tree.KD where

import           Control.Monad.Primitive
import           Data.Vector.Algorithms.Intro
import qualified Data.Vector.Generic          as Vector
import qualified Data.Vector.Generic.Mutable          as MVector
import           Control.Monad.ST

data Tree v a
  = Tree
  { accessors :: [a -> a -> Ordering]
  , storage   :: v a
  }

buildTree :: (Vector.Vector v a) => [a -> a -> Ordering] -> v a -> Tree v a
buildTree (cmps :: [a -> a -> Ordering]) (xs' :: v a) = Tree cmps (Vector.modify f xs')
  where
    f :: Vector.Mutable v s a -> ST s ()
    f (xs :: Vector.Mutable v s a) = go (cycle cmps) 0 (Vector.length xs')
      where
        go :: [a -> a -> Ordering] -> Int -> Int -> ST s ()
        go (c:cs) l u
          | l + 1 >= u = pure ()
          | otherwise = selectByBounds c xs k l u *> go cs l k *> go cs (k+1) u
          where k = (u-l) `div` 2
        go [] _ _ = error "empty comparison list"
