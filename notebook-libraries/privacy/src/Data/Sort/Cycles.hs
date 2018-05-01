module Data.Sort.Cycles where

import Data.Semigroup
import GHC.Base (oneShot)

import qualified Data.Map as Map
import           Data.Foldable
import           Control.Applicative.Backwards
import           Control.Monad.State
import           Type.Compose
import           Control.Applicative
import           Data.Coerce.Utilities
import           Control.Lens
import           Control.Arrow
import Data.Tuple
import qualified Data.Array.IArray as Array
import Data.Functor.Reverse

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

data Tree :: * -> * where
    Leaf :: {-# UNPACK #-} !Int -> Tree Int
    (:*:) :: !(Tree a) -> !(Tree a) -> Tree a

instance Foldable Tree where
    foldr f = go
      where
        go b (Leaf x) = f x b
        go b (xs :*: ys) = go (go b ys) xs
    {-# INLINE foldr #-}

sortPermuteInds :: Ord a => [a] -> (Int, [(Int,Int)])
sortPermuteInds xs = (ln, ys)
  where
    (ln,mp) = foldl' f (0, Map.empty) xs
    f (i,m) x = (i + 1, Map.alter (Just . maybe [i] (i :)) x m)
    ys = ifoldrOf folded (\i e zs -> (e, i) : zs) [] (Compose (Reverse <#$> mp))

-- |
-- >>> sortInds "aabcda"
-- [0,1,3,4,5,2]
sortInds :: Ord a => [a] -> [Int]
sortInds xs = Array.elems vs
  where
    (ln,ys) = sortPermuteInds xs
    vs :: Array.Array Int Int
    vs = Array.array (0,ln - 1) ys

sortCycles
    :: (Ord a, Traversable f, Applicative f)
    => [f a] -> [f Int]
sortCycles = getZipList
          #. knot
           . first (appEndo . getDual)
          #. getCompose
          #. forwards
          #. getCompose
          #. traverse (Compose #. fmap ZipList #. Backwards #. Compose #. first (Dual . Endo) #. f)
           . sequenceA
  where
    f x = (fw, state go)
      where
        fw = sortInds . zip x
        go y = (map fst zs, Array.elems bw)
          where
            (ln,zs) = sortPermuteInds (zip x y)
            bw :: Array.Array Int Int
            bw = Array.array (0, ln - 1) zs
    knot (s,t) = evalState t (s (repeat 0))

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
