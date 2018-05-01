{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}


module Main (main) where

import           Control.Applicative
import           Numeric.Peano
import           Data.Coerce.Utilities

import           Data.List                    (sortOn)
import           Data.Sort.Cycles

import           Hedgehog
import qualified Hedgehog.Gen                 as Gen
import qualified Hedgehog.Range               as Range

import           Privacy.Test.Generators.List
import           Privacy.Test.Data

slowSortCycles :: (Ord a, Traversable f, Applicative f) => [f a] -> [f Int]
slowSortCycles = getZipList
              #. traverse (ZipList #. map fst . sortOn snd . zip [0 ..])
               . traverse rotations

prop_sortCyclesSameAsSlow :: Property
prop_sortCyclesSameAsSlow =
    property $
    do sortCycles cyclicTupleInput === cyclicTupleExpect
       xs <- forAll (Gen.list (Range.linear 0 10) (genList @ (N 3) @ Ordering Gen.enumBounded))
       slowSortCycles xs === sortCycles xs

main :: IO Bool
main = checkParallel $$(discover)
