module Data.Heap.Pairing.Frequencies where

import Data.Semigroup (Semigroup((<>)))

data Heap a = Heap {-# UNPACK #-} !Int !a [Heap a]

instance Semigroup (Heap a) where
    Heap i x xs <> Heap j y ys
      | i <= j = Heap i x (Heap j y ys : xs)
      | otherwise = Heap j y (Heap i x xs : ys)

minView :: Heap a -> (Int, a, Maybe (Heap a))
minView (Heap i x xs) = (i, x, case xs of
    [] -> Nothing
    (y:ys) -> Just (mergeHeaps y ys))
  where
    mergeHeaps t []          = t
    mergeHeaps t1 [t2]       = t1 <> t2
    mergeHeaps t1 (t2:t3:ts) = (t1 <> t2) <> mergeHeaps t3 ts

insertHeap :: Int -> a -> Heap a -> Heap a
insertHeap i x (Heap j y ys)
  | i <= j = Heap i x [Heap j y ys]
  | otherwise = Heap j y (Heap i x [] : ys)