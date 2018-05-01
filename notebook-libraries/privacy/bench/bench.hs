module Main (main) where

import Criterion.Main
import System.Random
import Control.Monad
import Control.Monad.State.Bidirectional

int :: Int -> IO Int
int n = randomRIO (0,n)

benchAtSize :: Int -> Benchmark
benchAtSize n =
    env (replicateM n (int n)) $
    \xs ->
         bgroup (show n) [bench "rotations" $ nf rotations xs]

main :: IO ()
main = defaultMain (map benchAtSize [100])
