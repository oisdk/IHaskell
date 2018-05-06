module Main (main) where

import           Control.Applicative
import           Control.Monad
import           Criterion.Main
import           Data.Sort.Cycles                         hiding (rotations)
import           Data.Sort.Small
import           Data.Sort.Medians
import           System.Random
import qualified Data.Vector as Vector

import           Control.Monad.State.Bidirectional
import qualified Control.Monad.State.Bidirectional.Church as Church

int :: Int -> IO Int
int n = randomRIO (0,n)

listFive :: Int -> IO (ZipList Int)
listFive = fmap ZipList . replicateM 5 . int

uncurry5 f (v,w,x,y,z) = f v w x y z

smallSortBench :: Benchmark
smallSortBench =
    env ((,,,,) <$> int n <*> int n <*> int n <*> int n <*> int n) $
    \xs ->
         bench "smallSort5" $ nf (uncurry5 (sort5 (<=))) xs
  where
    n = 10


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
                   , bench "quick" $ nf fastMedian xs]
        , env (replicateM n (listFive n)) $
          \xs ->
               bgroup
                   "sorts"
                   [ bench "optimized" $ nf sortCycles xs
                   , bench "slow" $ nf slowSortCycles xs]]

main :: IO ()
main = defaultMain [smallSortBench] --(map benchAtSize [10000])
