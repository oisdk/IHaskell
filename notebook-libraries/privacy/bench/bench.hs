module Main (main) where

import Criterion.Main
import System.Random
import Control.Monad
import Data.Sort.Cycles
import Control.Applicative

int :: Int -> IO Int
int n = randomRIO (0,n)

listFive :: Int -> IO (ZipList Int)
listFive = fmap ZipList . replicateM 5 . int

benchAtSize :: Int -> Benchmark
benchAtSize n =
    bgroup
        (show n)
        [ env (replicateM n (int n)) $
          \xs ->
               bench "rotations" $ nf rotations xs
        , env (replicateM n (listFive n)) $
          \xs ->
               bgroup
                   "sorts"
                   [ bench "optimized" $ nf sortCycles xs
                   , bench "slow" $ nf slowSortCycles xs]]

main :: IO ()
main = defaultMain (map benchAtSize [1000])
