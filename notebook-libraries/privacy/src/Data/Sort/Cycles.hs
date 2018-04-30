module Data.Sort.Cycles where

import Data.Semigroup
import GHC.Base (oneShot)

import qualified Data.Set as Set
import qualified Data.Map as Map
import           Data.Maybe
import           Data.List.Indexed
import           Data.List.NonEmpty (NonEmpty(..), nonEmpty)
import           Data.Foldable
import           Data.Traversable

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

{-# ANN sortCycles "HLint: ignore Too strict maybe" #-}
sortCycles
    :: Ord a
    => [List n a] -> [List n Int]
sortCycles = maybe [] (untranspose . snd . flip g Nothing . transpose) . nonEmpty
  where
    b a = (fromJust a, Nil)

    g :: forall n a. Ord a => List n (NonEmpty a) -> Maybe [Int] -> ([Int], List n [Int])
    g Nil = b
    g (x :- xs) = f (toList x) (g xs)

    f x xs a = (map (`Map.findIndex` z) zipped, foldMap ($ []) z :- ys)
      where
        (y,ys) = xs . Just $ maybe (sortInds x) (sortInds . flip zip x) a
        zipped = zip x y
        z = Map.fromListWith (flip (.)) (zipWith (\e i -> (e, (:) i)) zipped [0 ..])

    sortInds xs = map (`Set.findIndex` ys) xs
      where
        ys = Set.fromList xs
