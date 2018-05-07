module Data.Sort.Medians where

import qualified Data.Vector.Generic as Vector
import qualified Data.Vector.Generic.Mutable as MVector
import qualified Data.Vector.Algorithms.Insertion as Insertion

import Control.Monad.Primitive
import Control.Monad.ST

import Data.Bool

import           Data.Sort.Small

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
selectBy cmp !i (xs :: v a) =
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
        _ -> case compare i ltl of
          GT -> selectBy cmp (i - ltl - 1) gt
          EQ -> pivot
          LT -> selectBy cmp i lt
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
    (gt,lt) = unstablePartition1 cmp pivot xs
    ltl = Vector.length lt
{-# INLINE selectBy #-}

unstablePartitionM :: forall m v a. (PrimMonad m, MVector.MVector v a)
                  => (a -> a -> Bool) -> a -> v (PrimState m) a -> m Int
{-# INLINE unstablePartitionM #-}
unstablePartitionM cmp e !v = from_left 0 0 (MVector.length v)
  where
    from_left :: Int -> Int -> Int -> m Int
    from_left k i j
      | i == j    = i <$ (MVector.unsafeRead v (i-1) >>= MVector.unsafeWrite v k)
      | otherwise = do
                      x <- MVector.unsafeRead v i
                      if cmp e x
                        then from_left (bool k i (cmp x e)) (i+1) j
                        else from_right k i (j-1)

    from_right :: Int -> Int -> Int -> m Int
    from_right k i j
      | i == j    = i <$ (MVector.unsafeRead v (i-1) >>= MVector.unsafeWrite v k)
      | otherwise = do
                      x <- MVector.unsafeRead v j
                      if cmp e x
                        then do
                               y <- MVector.unsafeRead v i
                               MVector.unsafeWrite v i x
                               MVector.unsafeWrite v j y
                               from_left (bool k i (cmp x e)) (i+1) j
                        else from_right k i (j-1)

-- |
-- >>> unstablePartition1 (<=) 3 (V.fromList [1,2,3,4,5,6] :: V.Vector Int)
-- ([6,5,4],[2,1])
unstablePartition1 :: Vector.Vector v a => (a -> a -> Bool) -> a -> v a -> (v a, v a)
unstablePartition1 cmp e xs = runST $ do
        mv <- Vector.thaw xs
        i <- unstablePartitionM cmp e mv
        v <- Vector.unsafeFreeze mv
        return (Vector.unsafeTake (i-1) v, Vector.unsafeDrop i v)
{-# INLINE unstablePartition1 #-}

-- $setup
-- >>> import qualified Data.Vector as V
