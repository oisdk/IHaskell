module Data.Statistics.Lens where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Coerce
import Control.Lens
import Data.Strict.Pair
import GHC.Base (oneShot)
import Data.Monoid (Sum(..))

import Statistics.Distribution.Normal (normalDistr)
import Statistics.Distribution (genContVar)
import System.Random.MWC (withSystemRandom)

newtype MonoidMap a b = MonoidMap
    { getMonoidMap :: Map a b
    }

instance (Ord a, Monoid b) =>
         Monoid (MonoidMap a b) where
    mempty = MonoidMap Map.empty
    {-# INLINE mempty #-}
    mappend =
        (coerce :: (Map a b -> Map a b -> Map a b) -> MonoidMap a b -> MonoidMap a b -> MonoidMap a b)
            (Map.unionWith mappend)
    {-# INLINE mappend #-}

type Median a = Maybe (Either a (a,a))

weightedMedianOf
    :: (Monoid b, Ord b)
    => Getting (MonoidMap a b :!: b) s (a, b) -> s -> Median a
weightedMedianOf l xs = Map.foldrWithKey f (const Nothing) mp mempty
  where
    f x y k =
        oneShot
            (\ !a ->
                  let !lb = a `mappend` y
                  in if lb >= n
                         then Just (Left x)
                         else let !ub = lb `mappend` y
                              in case compare ub n of
                                     LT -> k ub
                                     EQ ->
                                         case k ub of
                                             Just (Left z) ->
                                                 Just (Right (x, z))
                                             _ ->
                                                 errorWithoutStackTrace
                                                     "Data.Statistics.Lens.medianOf: bug!"
                                     GT -> Just (Left x))
    (MonoidMap mp :!: n) =
        foldMapOf
            l
            (\(x,y) ->
                  MonoidMap (Map.singleton x y) :!: y)
            xs

countedMedianOf :: Getting (MonoidMap a (Sum Int) :!: Sum Int) s (a, Int) -> s -> Median a
countedMedianOf ln = weightedMedianOf (ln . seconding (_Unwrapping Sum))

weightedMedian :: (Ord a, Monoid b, Ord b, Foldable f) => f (a,b) -> Median a
weightedMedian = weightedMedianOf folded

countedMedian :: (Ord a, Foldable f) => f (a,Int) -> Median a
countedMedian = countedMedianOf folded

medianOf :: Getting (MonoidMap a (Sum Int) :!: Sum Int) s a -> s -> Median a
medianOf ln = countedMedianOf (ln . to (flip (,) 1))

median :: (Foldable f, Ord a) => f a -> Median a
median = medianOf folded

normalDist :: Double -> Double -> IO Double
normalDist mean stddev =
    withSystemRandom
        (genContVar (normalDistr mean stddev) `asTypeOf`
         const (undefined :: IO Double))
