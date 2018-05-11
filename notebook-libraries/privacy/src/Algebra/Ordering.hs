module Algebra.Ordering
  (Comparison(..)
  ,smaller
  ,lte
  ,Contravariant(..)
  ,(>$<)
  ,(>$$<)
  ,($<))
  where

import Data.Functor.Contravariant

smaller :: Comparison a -> a -> a -> Bool
smaller (Comparison cmp) x y = cmp x y == LT

lte :: Comparison a -> a -> a -> Bool
lte (Comparison cmp) x y = cmp x y /= GT
