module Algebra.Information.Huffman where

import Algebra.Information.Tree
import Algebra.Information.Histogram
import Algebra.Ordering

import Data.Heap.Pairing
import Data.Coerce.Utilities

import Data.Semigroup
import Data.Semigroup.Foldable
import Data.Maybe

import qualified Data.Map.Strict as Map

buildHuffman :: Semigroup a
             => Comparison a
             -> Heap a (Tree a b) -> Tree a b
buildHuffman order hp =
    case minView (lte order) hp of
        (_,x,Nothing) -> x
        (i,x,Just xs) ->
            case minView (lte order) xs of
                (j,y,Nothing) -> Node (i <> j) x y
                (j,y,Just ys) ->
                    buildHuffman
                        order
                        (insertHeap (lte order) ij (Node ij x y) ys)
                    where ij = i <> j

buildHeap :: Comparison a -> Histogram a b -> Heap a (Tree a b)
buildHeap order
    = fromJust
    . Map.foldlWithKey' f Nothing
    .# getHistogram
  where
    f hp k v = Just (maybe (singleton v leaf)
                           (insertHeap (lte order) v leaf) hp)
      where
        leaf = Leaf v k

huffmanTree :: (Ord a, Semigroup b, Foldable1 f)
            => Comparison b
            -> (a -> b)
            -> f a
            -> Tree b a
huffmanTree order info
    = buildHuffman order
    . buildHeap order
    . foldMap1 (histogramOf info)

