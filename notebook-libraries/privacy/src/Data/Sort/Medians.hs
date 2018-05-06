module Data.Sort.Medians where

import qualified Data.Vector.Generic as Vector
import qualified Data.Vector.Algorithms.Insertion as Insertion
import Data.Sort.Small

naiveMedian :: (Vector.Vector v a, Ord a) => v a -> a
naiveMedian xs = ys Vector.! (Vector.length xs `div` 2)
  where
    ys = Vector.modify Insertion.sort xs
{-# INLINE naiveMedian #-}

fastMedian :: (Vector.Vector v a, Ord a) => v a -> a
fastMedian xs = select (Vector.length xs `div` 2) xs
{-# INLINE fastMedian #-}

select :: (Vector.Vector v a, Ord a) => Int -> v a -> a
select = selectBy (<=)
{-# INLINE select #-}

selectBy :: (Vector.Vector v a) => (a -> a -> Bool) -> Int -> v a -> a
selectBy cmp !i xs =
    case ln of
        0 -> error "Empty vector given to select"
        1 ->
            case i of
                0 -> Vector.head xs
                _ -> error "Select out of range"
        2 ->
            let (h,l) = (Vector.head xs, Vector.last xs)
            in if cmp h l
                   then case i of
                            0 -> h
                            1 -> l
                            _ -> error "select out of range"
                   else case i of
                            0 -> l
                            1 -> h
                            _ -> error "select out of range"
        3 ->
            sort3K
                (\_0 _1 _2 ->
                      case i of
                          0 -> _0
                          1 -> _1
                          2 -> _2
                          _ -> error "select out of range")
                cmp
                (xs Vector.! 0)
                (xs Vector.! 1)
                (xs Vector.! 2)
        4 ->
            sort4K
                (\_0 _1 _2 _3 ->
                      case i of
                          0 -> _0
                          1 -> _1
                          2 -> _2
                          3 -> _3
                          _ -> error "select out of range")
                cmp
                (xs Vector.! 0)
                (xs Vector.! 1)
                (xs Vector.! 2)
                (xs Vector.! 3)
        5 ->
            sort5K
                (\_0 _1 _2 _3 _4 ->
                      case i of
                          0 -> _0
                          1 -> _1
                          2 -> _2
                          3 -> _3
                          4 -> _4
                          _ -> error "select out of range")
                cmp
                (xs Vector.! 0)
                (xs Vector.! 1)
                (xs Vector.! 2)
                (xs Vector.! 3)
                (xs Vector.! 4)
        _ | i >= ltel -> selectBy cmp (i - ltel) gt
          | i >= ltl -> pivot
        _ -> selectBy cmp i lt
  where
    pivot =
        selectBy
            cmp
            (lnd `div` 2)
            (Vector.generate
                 lnd
                 (\j ->
                       let j' = j * 5
                       in if j /= lnd'
                              then median5
                                       cmp
                                       (xs Vector.! j')
                                       (xs Vector.! (j' + 1))
                                       (xs Vector.! (j' + 2))
                                       (xs Vector.! (j' + 3))
                                       (xs Vector.! (j' + 4))
                              else case lnm of
                                       1 -> xs Vector.! j'
                                       2 -> xs Vector.! j'
                                       3 ->
                                           median3
                                               cmp
                                               (xs Vector.! j')
                                               (xs Vector.! (j' + 1))
                                               (xs Vector.! (j' + 2))
                                       4 ->
                                           median4
                                               cmp
                                               (xs Vector.! j')
                                               (xs Vector.! (j' + 1))
                                               (xs Vector.! (j' + 2))
                                               (xs Vector.! (j' + 3))
                                       _ -> error "select: bug!") `asTypeOf`
             xs)
    ln = Vector.length xs
    (lnd',lnm) = ln `divMod` 5
    lnd
      | lnm == 0 = lnd'
      | otherwise = lnd' + 1
    (lte,gt) = Vector.unstablePartition (`cmp` pivot) xs
    lt = Vector.filter (not . cmp pivot) lte
    ltel = Vector.length xs - Vector.length gt
    ltl = Vector.length lt
{-# INLINE selectBy #-}
