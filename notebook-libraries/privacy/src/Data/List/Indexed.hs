{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}

module Data.List.Indexed where

import Numeric.Peano
import Data.Functor.Apply
import Data.List.NonEmpty (NonEmpty(..))
import Control.Applicative

infixr 5 :-
data List n a where
  Nil :: List Z a
  (:-) :: a -> List n a -> List (S n) a

deriving instance Functor (List n)
deriving instance Foldable (List n)
deriving instance Traversable (List n)
deriving instance Eq a => Eq (List n a)
deriving instance Ord a => Ord (List n a)
deriving instance Show a => Show (List n a)

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

type instance '(List,a) `OfSize` n = List n a

instance ByInductionOn n =>
         Applicative (List n) where
    pure x = induction (x :-) Nil
    {-# INLINE pure #-}
    (<*>) =
        induction
            (\r (f :- fs) (x :- xs) -> f x :- r fs xs)
            (\Nil Nil -> Nil)
    {-# INLINE (<*>) #-}
    liftA2 f =
        induction
            (\r (x :- xs) (y :- ys) -> f x y :- r xs ys)
            (\Nil Nil -> Nil)
    {-# INLINE liftA2 #-}

-- instance Show a => Show (List n a) where
--     showsPrec n = showsPrec n . toList
