{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}


module Main (main) where

import           Numeric.Peano

import           Data.Sort.Cycles
import           Data.Sort.Medians

import           Hedgehog
import qualified Hedgehog.Gen                 as Gen
import qualified Hedgehog.Range               as Range

import qualified Data.Vector as Vector

import           Privacy.Test.Generators.List
import           Privacy.Test.Data

prop_sortCyclesSameAsSlow :: Property
prop_sortCyclesSameAsSlow =
    property $
    do sortCycles cyclicTupleInput === cyclicTupleExpect
       xs <- forAll (Gen.list (Range.linear 0 10) (genList @ (N 3) @ Ordering Gen.enumBounded))
       slowSortCycles xs === sortCycles xs

prop_medians :: Property
prop_medians = property $ do
    xs <- forAll (Gen.list (Range.linear 1 100) (Gen.int (Range.linear 0 5)))
    let ys = Vector.fromList xs
    fastMedian ys === naiveMedian ys


main :: IO Bool
main = checkParallel $$(discover)
