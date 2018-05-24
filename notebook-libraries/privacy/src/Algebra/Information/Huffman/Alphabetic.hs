module Algebra.Information.Huffman.Alphabetic where

import Data.Nexus.Commutative
import Data.Semigroup
import Data.Semigroup.Foldable
import Data.Tree.Labelled
import Data.List (sortOn)
import Data.Foldable.Safe hiding (head)
import Data.Maybe
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Control.Monad.State

data PathLength = PathLength
    { totLength :: {-# UNPACK #-} !Int
    , numPaths  :: {-# UNPACK #-} !Int
    }

instance Semigroup PathLength where
    PathLength xl xn <> PathLength yl yn = PathLength (xl+xn+yl+yn) (xn+yn)

instance Monoid PathLength where
    mempty = PathLength 0 0
    mappend = (<>)

treeFirst :: Tree b a -> a
treeFirst = getFirst . foldMap1 First

treeLast :: Tree b a -> a
treeLast = getLast . foldMap1 Last

optAlphHuffman :: Ord a => [(a, Int)] -> Tree Int a
optAlphHuffman =
    snd .
    fromJust .
    minimumOn (totLength . fst) .
    enumerateTrees f .
    map
        (\(x,n) ->
               (PathLength 0 n, Leaf n x)) .
    sortOn fst
  where
    f (xn,x) (yn,y)
      | xf <= xl && xl <= yf && yf <= yl =
          [(xn <> yn, Node (measure x + measure y) x y)]
      | yf <= yl && yl <= xf && xf <= xl =
          [(xn <> yn, Node (measure x + measure y) y x)]
      | otherwise = []
      where
        xf = treeFirst x
        xl = treeLast x
        yf = treeFirst y
        yl = treeLast y

alphHuffman :: Map a Int -> Tree Int a
alphHuffman xs = evalState (go c) (Map.toList xs)
  where
    c = sum xs
    go n = do
      (x,i) <- gets head
      if i >= n
        then do
          modify tail
          pure (Leaf i x)
        else do
          let m = n `div` 2
          ls <- go m
          let nxt = n - measure ls
          if nxt <= 0 then pure ls else do
            rs <- go nxt
            pure (Node (measure ls + measure rs) ls rs)
