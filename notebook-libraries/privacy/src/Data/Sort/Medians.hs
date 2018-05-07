module Data.Sort.Medians (median,medianBy,select,selectBy) where

import qualified Data.Vector.Generic as Vector
import qualified Data.Vector.Generic.Mutable as MVector

import Control.Monad.Primitive
import Control.Monad.ST

import           Data.Sort.Small

median :: (Vector.Vector v a, Ord a) => v a -> a
median xs = select (Vector.length xs `div` 2) xs
{-# INLINE median #-}

medianBy :: (Vector.Vector v a) => (a -> a -> Ordering) -> v a -> a
medianBy cmp xs = selectBy cmp (Vector.length xs `div` 2) xs
{-# INLINE medianBy #-}

select :: (Vector.Vector v a, Ord a) => Int -> v a -> a
select = selectBy compare
{-# INLINE select #-}


selectBy :: (Vector.Vector v a) => (a -> a -> Ordering) -> Int -> v a -> a
selectBy cmp !i (xs :: v a) =
    case ln of
        0 -> errorWithoutStackTrace "Empty vector given to select"
        1 ->
            case i of
                0 -> Vector.unsafeHead xs
                _ -> errorWithoutStackTrace "Select out of range"
        2 ->
            let (h,l) = (Vector.unsafeHead xs, Vector.unsafeLast xs)
            in if lte h l
                   then case i of
                            0 -> h
                            1 -> l
                            _ -> errorWithoutStackTrace "select out of range"
                   else case i of
                            0 -> l
                            1 -> h
                            _ -> errorWithoutStackTrace "select out of range"
        3 ->
            sort3K
                (\_0 _1 _2 ->
                      case i of
                          0 -> _0
                          1 -> _1
                          2 -> _2
                          _ -> errorWithoutStackTrace "select out of range")
                lte
                (xs `Vector.unsafeIndex` 0)
                (xs `Vector.unsafeIndex` 1)
                (xs `Vector.unsafeIndex` 2)
        4 ->
            sort4K
                (\_0 _1 _2 _3 ->
                      case i of
                          0 -> _0
                          1 -> _1
                          2 -> _2
                          3 -> _3
                          _ -> errorWithoutStackTrace "select out of range")
                lte
                (xs `Vector.unsafeIndex` 0)
                (xs `Vector.unsafeIndex` 1)
                (xs `Vector.unsafeIndex` 2)
                (xs `Vector.unsafeIndex` 3)
        5 ->
            sort5K
                (\_0 _1 _2 _3 _4 ->
                      case i of
                          0 -> _0
                          1 -> _1
                          2 -> _2
                          3 -> _3
                          4 -> _4
                          _ -> errorWithoutStackTrace "select out of range")
                lte
                (xs `Vector.unsafeIndex` 0)
                (xs `Vector.unsafeIndex` 1)
                (xs `Vector.unsafeIndex` 2)
                (xs `Vector.unsafeIndex` 3)
                (xs `Vector.unsafeIndex` 4)
        _ -> case compare i ltl of
          GT -> selectBy cmp (i - ltl - 1) gt
          EQ -> pivot
          LT -> selectBy cmp i lt
  where
    lte x y = cmp x y /= GT
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
                                       lte
                                       (xs `Vector.unsafeIndex` j')
                                       (xs `Vector.unsafeIndex` (j' + 1))
                                       (xs `Vector.unsafeIndex` (j' + 2))
                                       (xs `Vector.unsafeIndex` (j' + 3))
                                       (xs `Vector.unsafeIndex` (j' + 4))
                              else case lnm of
                                       1 -> xs `Vector.unsafeIndex` j'
                                       2 -> xs `Vector.unsafeIndex` j'
                                       3 ->
                                           median3
                                               lte
                                               (xs `Vector.unsafeIndex` j')
                                               (xs `Vector.unsafeIndex` (j' + 1))
                                               (xs `Vector.unsafeIndex` (j' + 2))
                                       4 ->
                                           median4
                                               lte
                                               (xs `Vector.unsafeIndex` j')
                                               (xs `Vector.unsafeIndex` (j' + 1))
                                               (xs `Vector.unsafeIndex` (j' + 2))
                                               (xs `Vector.unsafeIndex` (j' + 3))
                                       _ -> errorWithoutStackTrace "select: bug!") `asTypeOf`
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
                  => (a -> a -> Ordering) -> a -> Int -> Int ->  v (PrimState m) a -> m Int
{-# INLINE unstablePartitionM #-}
unstablePartitionM cmp e lb ub !v = from_left 0 lb ub
  where
    from_left :: Int -> Int -> Int -> m Int
    from_left !k i j
      | i == j = i <$ MVector.unsafeSwap v k (i - 1)
      | otherwise = do
          x <- MVector.unsafeRead v i
          case cmp e x of
              LT -> from_left k (i + 1) j
              EQ -> from_left i (i + 1) j
              GT -> from_right k i (j - 1)
    from_right :: Int -> Int -> Int -> m Int
    from_right !k i j
      | i == j = i <$ MVector.unsafeSwap v k (i - 1)
      | otherwise = do
          x <- MVector.unsafeRead v j
          case cmp e x of
              LT -> do
                  y <- MVector.unsafeRead v i
                  MVector.unsafeWrite v i x
                  MVector.unsafeWrite v j y
                  from_left k (i + 1) j
              EQ -> do
                  y <- MVector.unsafeRead v i
                  MVector.unsafeWrite v i x
                  MVector.unsafeWrite v j y
                  from_left i (i + 1) j
              GT -> from_right k i (j - 1)

-- |
-- >>> unstablePartition1 compare 3 (V.fromList [1,2,3,4,5,6] :: V.Vector Int)
-- ([6,5,4],[2,1])
unstablePartition1 :: Vector.Vector v a => (a -> a -> Ordering) -> a -> v a -> (v a, v a)
unstablePartition1 cmp e xs = runST $ do
        mv <- Vector.thaw xs
        i <- unstablePartitionM cmp e 0 (Vector.length xs) mv
        v <- Vector.unsafeFreeze mv
        return (Vector.unsafeTake (i-1) v, Vector.unsafeDrop i v)
{-# INLINE unstablePartition1 #-}

-- $setup
-- >>> import qualified Data.Vector as V
