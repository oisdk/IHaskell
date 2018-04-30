module Data.Sort.Cycles where

import Data.Semigroup
import GHC.Base (oneShot)

import qualified Data.Set as Set
import qualified Data.Map as Map
import           Data.Foldable
import           Control.Applicative.Backwards
import           Control.Monad.State
import           Data.Functor.Compose
import           Control.Applicative
import           Data.Coerce.Utilities

cmpSlide
    :: Ord a
    => [a] -> [a] -> [Ordering]
cmpSlide xs' ys' = snd (foldr g b (zipWith compare xs' ys') EQ)
  where
    g x xs =
        oneShot
            (\ a ->
                  let (y,ys) = xs (a <> x)
                      !z = x <> y
                  in (z, z : ys))
    {-# INLINE g #-}
    b a = (a, [])
    {-# INLINE b #-}
{-# INLINABLE cmpSlide #-}

data Tree a = Leaf a | Tree a :*: Tree a deriving Foldable

sortCycles
    :: (Ord a, Traversable f, Applicative f)
    => [f a] -> [f Int]
sortCycles = getZipList #. knot . getCompose . forwards . getCompose . traverse f . sequenceA
  where
    f x = Compose (Backwards (Compose (Endo fw, state go)))
      where
        fw = sortInds . flip zip x
        go y = (ZipList (toList (Compose zs)), bw)
          where
            xy = zip x y
            zs = Map.fromListWith (:*:) (zipWith (\e -> (,) e . Leaf) xy [0..])
            bw = fmap (`Map.findIndex` zs) xy
    sortInds xs = fmap (`Set.findIndex` ys) xs
      where
        ys = Set.fromList xs
    knot (s,t) = evalState t (appEndo s (repeat 0))
