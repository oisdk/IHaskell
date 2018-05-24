module Data.Coerce.Utilities
  (upon
  ,(<#$>)
  ,(#.)
  ,(.#))
  where

import Data.Coerce
import Data.Profunctor.Unsafe

infixr 1 `upon`
upon :: Coercible a b => (b -> b -> b) -> (a -> b) -> a -> a -> a
upon f _ = coerce f
{-# INLINE upon #-}

infixl 4 <#$>
(<#$>) :: Coercible (f a) (f b) => (a -> b) -> f a -> f b
(<#$>) _ = coerce
{-# INLINE (<#$>) #-}
