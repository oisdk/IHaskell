module Algebra.Information.Huffman where

import Data.Tree.Labelled
import Data.Histogram

import Data.Heap.Pairing
import Data.Coerce.Utilities

import Data.Semigroup
import Data.Maybe

import qualified Data.Map.Strict as Map

buildHuffman :: Semigroup a
             => (a -> a -> Bool)
             -> Heap a (Tree a b) -> Tree a b
buildHuffman lte hp =
    case minView lte hp of
        (_,x,Nothing) -> x
        (i,x,Just xs) ->
            case minView lte xs of
                (j,y,Nothing) -> Node (i <> j) x y
                (j,y,Just ys) ->
                    buildHuffman
                        lte
                        (insertHeap lte ij (Node ij x y) ys)
                    where ij = i <> j

buildHeap :: (a -> a -> Bool) -> Histogram a b -> Heap a (Tree a b)
buildHeap lte
    = fromJust
    . Map.foldlWithKey' f Nothing
    .# getHistogram
  where
    f hp k v = Just (maybe (singleton v leaf)
                           (insertHeap lte v leaf) hp)
      where
        leaf = Leaf v k

huffmanTree :: (Ord a, Semigroup b, Foldable f)
            => (b -> b -> Bool)
            -> (a -> b)
            -> f a
            -> Tree b a
huffmanTree lte info
    = buildHuffman lte
    . buildHeap lte
    . foldMap (\x -> Histogram (Map.singleton x (info x)))

