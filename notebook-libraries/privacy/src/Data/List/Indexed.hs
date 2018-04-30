{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}

module Data.List.Indexed where

import Numeric.Peano
import Data.Functor.Apply
import Data.List.NonEmpty (NonEmpty(..))

infixr 5 :-
data List n a where
  Nil :: List Z a
  (:-) :: a -> List n a -> List (S n) a

deriving instance Functor (List n)
deriving instance Foldable (List n)
deriving instance Traversable (List n)
deriving instance Eq a => Eq (List n a)
deriving instance Ord a => Ord (List n a)

instance Apply (List n) where
    liftF2 _ Nil Nil = Nil
    liftF2 f (x :- xs) (y :- ys) = f x y :- liftF2 f xs ys

transpose :: NonEmpty (List n a) -> List n (NonEmpty a)
transpose (x :| xs) = h x $ foldr f Nothing xs
  where
    f y = Just . maybe (fmap pure y) (liftF2 (:) y)
    h y = maybe (fmap pure y) (liftF2 (:|) y)

untranspose :: List n [a] -> [List n a]
untranspose Nil = []
untranspose (x :- Nil) = map (:- Nil) x
untranspose (x :- xs) = zipWith (:-) x (untranspose xs)
