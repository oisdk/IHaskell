{-# options_ghc -fsimpl-tick-factor=200 #-}

module Main (main) where

import           Control.Applicative
import           Control.Monad
import           Criterion.Main
import           Data.Sort.Cycles                         hiding (rotations)
import           Data.Sort.Medians
import           System.Random
import qualified Data.Vector.Unboxed as Vector
import qualified Data.Vector.Algorithms.Heap as Vector

import           Control.Monad.State.Bidirectional
import qualified Control.Monad.State.Bidirectional.Church as Church

int :: Int -> IO Int
int n = randomRIO (0,n)

listFive :: Int -> IO (ZipList Int)
listFive = fmap ZipList . replicateM 5 . int


naiveMedian :: Vector.Vector Int -> Int
naiveMedian xs = Vector.modify Vector.sort xs Vector.! (Vector.length xs `div` 2)

benchAtSize :: Int -> Benchmark
benchAtSize n =
    bgroup
        (show n)
        [ env (replicateM n (int n)) $
          \xs ->
               bgroup
                   "rotations"
                   [ bench "tupled" $ nf rotations xs
                   , bench "church" $ nf Church.rotations xs]
        , env (Vector.fromList <$> replicateM n (int n)) $
          \xs ->
               bgroup
                   "medians"
                   [ bench "naive" $ nf naiveMedian xs
                   , bench "quick" $ nf median xs]
        , env (replicateM n (listFive n)) $
          \xs ->
               bgroup
                   "sorts"
                   [ bench "optimized" $ nf sortCycles xs
                   , bench "slow" $ nf slowSortCycles xs]]

main :: IO ()
main = defaultMain (map benchAtSize [10000, 1000000, 10000000])
