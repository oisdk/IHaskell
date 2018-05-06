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
import           Data.Tree.KD

import           Hedgehog
import qualified Hedgehog.Gen                 as Gen
import qualified Hedgehog.Range               as Range

import qualified Data.Vector as Vector
import qualified Data.Set as Set
import           Data.Ord
import           Data.List.NonEmpty (NonEmpty(..))
import           Data.List (sort)
import           Control.Monad
import           Control.Monad.ST

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

-- prop_smallMedians :: Property
-- prop_smallMedians = property $ do
--     xs <- forAll (replicateM 5 $ Gen.int (Range.linear 0 6))
--     footnote "Size 5"
--     let m5 = runST (median5 compare 0 =<< Vector.thaw (Vector.fromList xs))
--     m5 === (sort xs !! 2)
--     footnote "Size 4"
--     let m4 = runST (median4 compare 0 =<< Vector.thaw (Vector.fromList (tail xs)))
--     assert $ m4 == (sort (tail xs) !! 2) || m4 == (sort (tail xs) !! 1)
--     footnote "Size 3"
--     let m3 = runST (median3 compare 0 =<< Vector.thaw (Vector.fromList (tail (tail xs))))
--     assert $ m3 == (sort (tail (tail xs)) !! 1)


-- prop_kdTree :: Property
-- prop_kdTree =
--     property $
--     do xs <-
--            forAll
--                (Gen.list
--                     (Range.linear 1 100)
--                     ((,) <$> Gen.int (Range.linear 0 10) <*>
--                      Gen.int (Range.linear 0 10)))
--        let ks =
--                buildTree
--                    (comparing fst :| comparing snd : [])
--                    (Vector.fromList xs)
--        x <- forAll (Gen.element xs)
--        assert (member x ks)
--        let ss = Set.fromList xs
--        y <-
--            forAll
--                (Gen.filter (`Set.notMember` ss) $
--                 ((,) <$> Gen.int (Range.linear (-5) 15) <*>
--                  Gen.int (Range.linear (-5) 15)))
--        assert (not (member y ks))

main :: IO Bool
main = checkParallel $$(discover)
