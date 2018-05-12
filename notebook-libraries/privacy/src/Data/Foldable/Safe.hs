module Data.Foldable.Safe
  (head
  ,last
  ,foldr1
  ,foldl1
  ,foldr1'
  ,foldl1'
  ,minimumBy
  ,maximumBy
  ,minimumOn
  ,maximumOn
  ,minimum
  ,maximum)
  where

import           Control.Lens
import           Data.Foldable     (foldr', foldl')
import           Data.Strict.Maybe
import           Prelude           hiding (foldl1, foldr1, head, last,minimum,maximum)
import           Data.Semigroup

foldr1 :: Foldable f => (a -> a -> a) -> f a -> Maybe a
foldr1 f = foldr (\x -> Just . maybe x (f x)) Nothing

foldl1 :: Foldable f => (a -> a -> a) -> f a -> Maybe a
foldl1 f = foldl (\y x -> Just (maybe x (`f` x) y)) Nothing

foldr1' :: Foldable f => (a -> a -> a) -> f a -> Maybe a
foldr1' f = view lazy . foldr' (\x -> Just' . maybe' x (f x)) Nothing'

foldl1' :: Foldable f => (a -> a -> a) -> f a -> Maybe a
foldl1' f = view lazy . foldl' (\y x -> Just' (maybe' x (`f` x) y)) Nothing'

head :: Foldable f => f a -> Maybe a
head = foldr1 const

last :: Foldable f => f a -> Maybe a
last = foldl1 (flip const)

minimumBy :: Foldable f => (a -> a -> Ordering) -> f a -> Maybe a
minimumBy cmp =
    foldl1
        (\a e ->
              case cmp a e of
                  GT -> e
                  _ -> a)

maximumBy :: Foldable f => (a -> a -> Ordering) -> f a -> Maybe a
maximumBy cmp =
    foldl1
        (\a e ->
              case cmp a e of
                  LT -> e
                  _ -> a)

minimumOn :: (Foldable f, Ord b) => (a -> b) -> f a -> Maybe a
minimumOn k = f . foldMap (\x -> Option (Just (Min (Arg (k x) x))))
  where
    f (Option Nothing) = Nothing
    f (Option (Just (Min (Arg _ x)))) = Just x

maximumOn :: (Foldable f, Ord b) => (a -> b) -> f a -> Maybe a
maximumOn k = f . foldMap (\x -> Option (Just (Max (Arg (k x) x))))
  where
    f (Option Nothing) = Nothing
    f (Option (Just (Max (Arg _ x)))) = Just x

minimum :: (Foldable f, Ord a) => f a -> Maybe a
minimum = foldl1' min

maximum :: (Foldable f, Ord a) => f a -> Maybe a
maximum = foldl1' max
