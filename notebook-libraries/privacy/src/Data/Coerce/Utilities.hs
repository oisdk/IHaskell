module Data.Coerce.Utilities
  (ala
  ,(<#$>)
  ,(#.)
  ,(.#))
  where

import Data.Coerce
import Data.Profunctor.Unsafe

infixr 1 `ala`
ala :: Coercible a b => (b -> b -> b) -> (a -> b) -> a -> a -> a
ala f _ = coerce f
{-# INLINE ala #-}

infixl 4 <#$>
(<#$>) :: Coercible (f a) (f b) => (a -> b) -> f a -> f b
(<#$>) _ = coerce
{-# INLINE (<#$>) #-}
