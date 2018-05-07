module Small (sort5,sort5ST) where

import GHC.Exts (inline)
import Data.Vector.Mutable
import Control.Monad.ST
import Control.Monad

swap2 :: (a -> a -> Bool) -> a -> a -> (a -> a -> b) -> b
swap2 lte x y k
    | inline lte x y = inline k x y
    | otherwise = inline k y x
{-# INLINE swap2 #-}

sort5 :: (a -> a -> Bool) -> (a,a,a,a,a) -> (a,a,a,a,a)
sort5 lte ~(_0,_1,_2,_3,_4)
    = swap2 lte _0 _1 $ \ _0 _1 ->
      swap2 lte _2 _3 $ \ _2 _3 ->
      swap2 lte _0 _2 $ \ _0 _2 ->
      swap2 lte _1 _3 $ \ _1 _3 ->
      swap2 lte _1 _2 $ \ _1 _2 ->
      swap2 lte _0 _4 $ \ _0 _4 ->
      swap2 lte _1 _4 $ \ _1 _4 ->
      swap2 lte _2 _4 $ \ _2 _4 ->
      swap2 lte _3 _4 $ \ _3 _4 ->
      (_0,_1,_2,_3,_4)
{-# INLINE sort5 #-}

swap2ST :: MVector s a -> (a -> a -> Bool) -> Int -> Int -> ST s ()
swap2ST ar lte x' y' = do
    x <- unsafeRead ar x'
    y <- unsafeRead ar y'
    unless (inline lte x y) $ do
        unsafeWrite ar x' y
        unsafeWrite ar y' x
{-# INLINE swap2ST #-}

sort5ST :: (a -> a -> Bool) -> (a,a,a,a,a) -> (a,a,a,a,a)
sort5ST lte (_0,_1,_2,_3,_4) = runST $ do
    ar <- new 5
    unsafeWrite ar 0 _0
    unsafeWrite ar 1 _1
    unsafeWrite ar 2 _2
    unsafeWrite ar 3 _3
    unsafeWrite ar 4 _4
    swap2ST ar lte 0 1
    swap2ST ar lte 2 3
    swap2ST ar lte 0 2
    swap2ST ar lte 1 3
    swap2ST ar lte 1 2
    swap2ST ar lte 0 4
    swap2ST ar lte 1 4
    swap2ST ar lte 2 4
    swap2ST ar lte 3 4
    _0' <- unsafeRead ar 0
    _1' <- unsafeRead ar 1
    _2' <- unsafeRead ar 2
    _3' <- unsafeRead ar 3
    _4' <- unsafeRead ar 4
    return (_0',_1',_2',_3',_4')
{-# INLINE sort5ST #-}
