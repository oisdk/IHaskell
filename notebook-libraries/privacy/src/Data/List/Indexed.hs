{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}

module Data.List.Indexed where

import Numeric.Peano
import Data.Functor.Apply
import Control.Applicative
import Data.Coerce.Utilities
import Data.Foldable

infixr 5 :-
data List n a where
  Nil :: List Z a
  (:-) :: !a -> !(List n a) -> List (S n) a

deriving instance Functor (List n)
deriving instance Foldable (List n)
deriving instance Traversable (List n)
deriving instance Eq a => Eq (List n a)
deriving instance Ord a => Ord (List n a)

instance Apply (List n) where
    liftF2 _ Nil Nil = Nil
    liftF2 f (x :- xs) (y :- ys) = f x y :- liftF2 f xs ys
    {-# INLINE liftF2 #-}

type instance '(List,a) ∝ n = List n a

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

head' :: List (S n) a -> a
head' (x :- _) = x
{-# INLINE head' #-}

tail' :: List (S n) a -> List n a
tail' (_ :- xs) = xs
{-# INLINE tail' #-}

instance ByInductionOn n =>
         Monad (List n) where
    (>>=) xs (f :: a -> List n b) =
        induction
            (\r (y :- ys) fn -> head' (fn (Const y)) :- r ys (tail' . fn .# Const .# getConst))
            (\Nil _ -> Nil)
            xs
            (f .# getConst :: Const a n -> List n b)
    {-# INLINE (>>=) #-}

instance Show a => Show (List n a) where
    showsPrec n = showsPrec n . toList
