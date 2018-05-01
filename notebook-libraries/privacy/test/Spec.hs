{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}


module Main (main) where

import           Numeric.Peano

import           Data.Sort.Cycles

import           Hedgehog
import qualified Hedgehog.Gen                 as Gen
import qualified Hedgehog.Range               as Range

import           Privacy.Test.Generators.List
import           Privacy.Test.Data


prop_sortCyclesSameAsSlow :: Property
prop_sortCyclesSameAsSlow =
    property $
    do sortCycles cyclicTupleInput === cyclicTupleExpect
       xs <- forAll (Gen.list (Range.linear 0 10) (genList @ (N 3) @ Ordering Gen.enumBounded))
       slowSortCycles xs === sortCycles xs

main :: IO Bool
main = checkParallel $$(discover)
