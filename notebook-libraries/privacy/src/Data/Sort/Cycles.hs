module Data.Sort.Cycles where

import Data.Semigroup
import GHC.Base (oneShot)

import qualified Data.Set as Set
import           Data.List (transpose)
import qualified Data.Map as Map
import           Data.Maybe

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

sortCycles
    :: Ord a
    => [[a]] -> [[Int]]
sortCycles = transpose . snd . flip (foldr f b) Nothing . transpose
  where
    b a = (fromJust a, [])
    f x xs a = (map (`Map.findIndex` z) zipped, foldMap ($ []) z : ys)
      where
        (y,ys) = xs . Just $ maybe (sortInds x) (sortInds . flip zip x) a
        zipped = zip x y
        z = Map.fromListWith (flip (.)) (zipWith (\e i -> (e, (:) i)) zipped [0 ..])
    sortInds xs = map (`Set.findIndex` ys) xs
      where
        ys = Set.fromList xs
