module Data.Sort.Cycles where

import Data.Semigroup
import GHC.Base (oneShot,build)

import qualified Data.Map.Strict as Map
import           Data.Foldable
import           Control.Applicative.Backwards
import           Control.Monad.State.Strict
import           Type.Compose
import           Control.Applicative
import           Data.Coerce.Utilities
import           Control.Lens
import qualified Data.Vector       as Vector
import qualified Data.Vector.Unboxed as UnboxedVector
import qualified Data.Vector.Unboxed.Mutable as MUnboxedVector

vecArray :: Int -> [(Int, a)] -> Vector.Vector a
vecArray i xs = Vector.replicate i undefined Vector.// xs
{-# INLINE vecArray #-}

uVecArray :: UnboxedVector.Unbox a => Int -> [(Int, a)] -> UnboxedVector.Vector a
uVecArray i = UnboxedVector.unsafeUpd (UnboxedVector.create (MUnboxedVector.unsafeNew i))
{-# INLINE uVecArray #-}

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

sortPermuteInds :: (Ord a, Foldable f) => f a -> (Int, [(Int,Int)])
sortPermuteInds xs = (ln, ys)
  where
    (ln,mp) = foldl' f (0, Map.empty) xs
    f (!i, !m) x = (i + 1, Map.alter (Just . maybe [i] (i :)) x m)
    {-# INLINE f #-}
    ys = build (\c n -> foldr (flip $ foldl' (\a e' -> oneShot (\ !i -> (e',i) `c` a (i+1)))) (const n) mp 0)
    {-# INLINE ys #-}
{-# INLINE sortPermuteInds #-}

-- |
-- >>> sortInds "aabcda"
-- [0,1,3,4,5,2]
sortInds :: (Ord a, Foldable f) => f a -> [Int]
sortInds = UnboxedVector.toList . uncurry uVecArray . sortPermuteInds
{-# INLINE sortInds #-}

sortCycles
    :: (Ord a, Traversable f, Applicative f)
    => [f a] -> [f Int]
sortCycles = (map.fmap) fst . sortCyclesInds
{-# INLINE sortCycles #-}

sortCyclesInds
    :: (Ord a, Traversable f, Applicative f)
    => [f a] -> [f (Int, Int)]
sortCyclesInds xs
    = getZipList
   #. traverse ZipList
    . snd . imapAccumROf traversed f ts
    . sequenceA
    $ xs
  where
    f 0 _ _ = (ts, hs)
    f _ y x = (UnboxedVector.toList bw, zs)
      where
        (ln,zs) = sortPermuteInds (zip x y)
        bw = uVecArray ln zs
    {-# INLINE f #-}
    (l,hs) = sortPermuteInds (map toList xs)
    ts = UnboxedVector.toList (uVecArray l hs)
{-# INLINE sortCyclesInds #-}

-- |
-- >>> rotations "abcd"
-- ["abcd","bcda","cdab","dabc"]
rotations :: Traversable t => t a -> t [a]
rotations = flip evalState id
          . forwards
         #. flip evalState id
          . getCompose
         #. traverse (Compose #. fmap Backwards #. f)
  where
    f x =
        state $
        \yl ->
             flip (,) (yl . (:) x) $
             state $
             \yr ->
                  let z = (:) x . yr
                  in (z (yl []), z)

slowSortCycles :: (Ord a, Traversable f, Applicative f) => [f a] -> [f Int]
slowSortCycles = (map.fmap) fst . slowSortCyclesInds

slowSortCyclesInds :: (Ord a, Traversable f, Applicative f) => [f a] -> [f (Int, Int)]
slowSortCyclesInds =
    getZipList #. traverse (ZipList #. f . sortPermuteInds) .
    traverse rotations
  where
    f (n,xs) = zipWith (\x y -> (fst x, y)) xs (UnboxedVector.toList (uVecArray n xs))
