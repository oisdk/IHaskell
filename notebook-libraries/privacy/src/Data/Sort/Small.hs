module Data.Sort.Small (sort5, sort4, sort3) where

import GHC.Base (inline)

swap2 :: (a -> a -> Bool) -> a -> a -> (a -> a -> b) -> b
swap2 cmp x y k
    | inline cmp x y = inline k x y
    | otherwise = inline k y x
{-# INLINE swap2 #-}

sort5K :: (a -> a -> a -> a -> a -> b) -> (a -> a -> Bool) -> a -> a -> a -> a -> a -> b
sort5K k cmp !_0 !_1 !_2 !_3 !_4
    = swap2 cmp _0 _1 $ \ !_0 !_1 ->
      swap2 cmp _2 _3 $ \ !_2 !_3 ->
      swap2 cmp _0 _2 $ \ !_0 !_2 ->
      swap2 cmp _1 _3 $ \ !_1 !_3 ->
      swap2 cmp _1 _2 $ \ !_1 !_2 ->
      swap2 cmp _0 _4 $ \ !_0 !_4 ->
      swap2 cmp _1 _4 $ \ !_1 !_4 ->
      swap2 cmp _2 _4 $ \ !_2 !_4 ->
      swap2 cmp _3 _4 $ \ !_3 !_4 ->
      inline k _0 _1 _2 _3 _4
{-# INLINE sort5K #-}

sort4K :: (a -> a -> a -> a -> b) -> (a -> a -> Bool) -> a -> a -> a -> a -> b
sort4K k cmp !_0 !_1 !_2 !_3
    = swap2 cmp _0 _1 $ \ !_0 !_1 ->
      swap2 cmp _2 _3 $ \ !_2 !_3 ->
      swap2 cmp _0 _2 $ \ !_0 !_2 ->
      swap2 cmp _1 _3 $ \ !_1 !_3 ->
      swap2 cmp _1 _2 $ \ !_1 !_2 ->
      inline k _0 _1 _2 _3
{-# INLINE sort4K #-}

sort5 :: (a -> a -> Bool) -> a -> a -> a -> a -> a -> (a,a,a,a,a)
sort5 = inline sort5K (,,,,)
{-# INLINE sort5 #-}

sort4 :: (a -> a -> Bool) -> a -> a -> a -> a -> (a, a, a, a)
sort4 = inline sort4K (,,,)
{-# INLINE sort4 #-}

sort3K :: (a -> a -> a -> b) -> (a -> a -> Bool) -> a -> a -> a -> b
sort3K k cmp !_0 !_1 !_2
    = swap2 cmp _1 _2 $ \ !_1 !_2 ->
      swap2 cmp _0 _2 $ \ !_0 !_2 ->
      swap2 cmp _0 _1 $ \ !_0 !_1 ->
      inline k _0 _1 _2
{-# INLINE sort3K #-}

sort3 :: (a -> a -> Bool) -> a -> a -> a -> (a, a, a)
sort3 = sort3K (,,)
{-# INLINE sort3 #-}
