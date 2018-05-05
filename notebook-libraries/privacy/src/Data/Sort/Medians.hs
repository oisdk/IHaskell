module Data.Sort.Medians where

import qualified Data.Vector.Generic as Vector
import qualified Data.Vector.Algorithms.Insertion as Insertion

import Data.Bool

naiveMedian :: (Vector.Vector v a, Ord a) => v a -> a
naiveMedian xs = ys Vector.! (Vector.length xs `div` 2)
  where
    ys = Vector.modify Insertion.sort xs
{-# INLINE naiveMedian #-}

fastMedian :: (Vector.Vector v a, Ord a) => v a -> a
fastMedian xs = select (Vector.length xs `div` 2) xs
{-# INLINE fastMedian #-}

select :: (Vector.Vector v a, Ord a) => Int -> v a -> a
select i xs
  | ln <= 5 = Vector.modify Insertion.sort xs Vector.! i
  | i < ltl = select i lt
  | i >= ltl && i - ltl < Vector.length eq = pivot
  | otherwise = select (i - Vector.length lte) gt
  where
    pivot =
        select
            (lnd `div` 2)
            (Vector.generate
                 lnd
                 (\j ->
                       naiveMedian
                           (Vector.slice
                                (j * 5)
                                (bool 5 lnm (j == lnd'))
                                xs)) `asTypeOf`
             xs)
    ln = Vector.length xs
    (lnd',lnm) = ln `divMod` 5
    lnd
      | lnm == 0 = lnd'
      | otherwise = lnd' + 1
    (lte,gt) = Vector.unstablePartition (<= pivot) xs
    (eq,lt) = Vector.unstablePartition (pivot ==) lte
    ltl = Vector.length lt
    eql = Vector.length eq
{-# INLINE select #-}
