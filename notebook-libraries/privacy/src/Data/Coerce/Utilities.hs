module Data.Coerce.Utilities where

import Data.Coerce

infixr 1 `ala`
ala :: Coercible a b => (b -> b -> b) -> (a -> b) -> a -> a -> a
ala f _ = coerce f
{-# INLINE ala #-}

infixr 9 #.
(#.) :: Coercible b c => (b -> c) -> (a -> b) -> a -> c
(#.) _ = coerce
{-# INLINE (#.) #-}

infixl 8 .#
(.#) :: Coercible a b => (b -> c) -> (a -> b) -> a -> c
(.#) f _ = coerce f
{-# INLINE (.#) #-}

infixl 4 <#$>
(<#$>) :: Coercible (f a) (f b) => (a -> b) -> f a -> f b
(<#$>) _ = coerce
{-# INLINE (<#$>) #-}
