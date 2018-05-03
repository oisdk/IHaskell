{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Privacy.Test.Generators.List where

import           Control.Applicative (liftA2)
import           Hedgehog              (Gen)

import           Data.List.Indexed
import           Numeric.Peano

type instance '(Gen,a) `OfSize` n = Gen (a `OfSize`n)

genList :: ByInductionOn n => Gen a -> Gen (List n a)
genList gen = induction (liftA2 (:-) gen) (pure Nil)
{-# INLINE genList #-}
