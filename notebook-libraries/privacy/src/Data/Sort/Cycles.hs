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
import           Control.Lens
import           Control.Arrow

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

-- |
-- >>> sortCycles input == expect
-- True
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
        go y = (toList (Compose zs), bw)
          where
            xy = zip x y
            zs = Map.fromListWith (:*:) (imap (\i e -> (e, Leaf i)) xy)
            bw = fmap (`Map.findIndex` zs) xy
    sortInds xs = fmap (`Set.findIndex` ys) xs
      where
        ys = Set.fromList xs
    knot (s,t) = evalState t (s (repeat 0))


-- $setup
-- >>> let expect = map ZipList [[11,13,9],[13,4,6],[0,5,1],[10,9,7],[3,0,13],[1,6,0],[9,1,12],[5,7,10],[4,10,4],[7,12,5],[14,2,14],[6,11,2],[12,14,11],[2,8,8],[8,3,3]]
-- >>> let input = map ZipList [[2,3,3],[5,4,2],[9,6,7],[4,7,9],[8,1,5],[7,2,6],[9,4,1],[8,4,2],[9,7,8],[6,3,1],[3,4,5],[1,6,8],[9,5,3],[2,1,3],[8,7,6]]
