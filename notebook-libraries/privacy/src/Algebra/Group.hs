module Algebra.Group where

class Monoid a => Group a where
    {-# MINIMAL inv | (<\) #-}
    inv :: a -> a
    inv x = mempty <\ x

    infixl 6 <\
    (<\) :: a -> a -> a
    x <\ y = x `mappend` inv y
