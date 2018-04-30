module Main (main) where

import Criterion.Main
import System.Random
import Control.Monad
import Data.Sort.Cycles
import Control.Applicative
int :: Int -> IO Int
int n = randomRIO (0,n)

benchAtSize :: Int -> Benchmark
benchAtSize n =
    env (liftA2 (,) (replicateM n (int n)) (replicateM n (int n))) $
    \xs ->
         bgroup (show n) [bench "cyclic" $ nf (uncurry cmpSlide) xs]

main :: IO ()
main = defaultMain (map benchAtSize [10000])
