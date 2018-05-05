{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Privacy.Test.Generators.List where

import           Control.Applicative (liftA2)
import           Hedgehog              (Gen)

import           Data.List.Indexed
import           Numeric.Peano

type instance '(Gen,a) ∝ n = Gen (a ∝ n)

genList :: Finite n => Gen a -> Gen (List n a)
genList gen = induction (pure Nil) (liftA2 (:-) gen)
{-# INLINE genList #-}
