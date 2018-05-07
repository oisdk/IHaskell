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
import           Data.Sort.Small

import           Hedgehog
import qualified Hedgehog.Gen                 as Gen
import qualified Hedgehog.Range               as Range

import qualified Data.Vector as Vector
import           Data.List (sort)

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
    i <- forAll (Gen.int (Range.linear 0 (Vector.length ys - 1)))
    selectBy compare i ys === (sort xs !! i)


prop_smallSorts :: Property
prop_smallSorts = property $ do
    let g = Gen.int (Range.linear 0 6)
    _0 <- forAll g
    _1 <- forAll g
    _2 <- forAll g
    _3 <- forAll g
    _4 <- forAll g
    sort5K (\v w x y z -> [v,w,x,y,z]) (<=) _0 _1 _2 _3 _4 === sort [_0,_1,_2,_3,_4]
    sort4K (\w x y z -> [w,x,y,z]) (<=) _0 _1 _2 _3 === sort [_0,_1,_2,_3]
    sort3K (\x y z -> [x,y,z]) (<=) _0 _1 _2 === sort [_0,_1,_2]
    median5 (<=) _0 _1 _2 _3 _4 === sort [_0,_1,_2,_3,_4] !! 2
    let m4 = median4 (<=) _0 _1 _2 _3
    let s4 = sort [_0,_1,_2,_3]
    assert $ m4 == s4 !! 2 || m4 == s4 !! 1
    median3 (<=) _0 _1 _2 === sort [_0,_1,_2] !! 1



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
