{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns    #-}

module Data.Heap.Pairing where

import Data.Foldable
import Data.Semigroup
import Data.Semigroup.Foldable
import Data.List.NonEmpty (NonEmpty(..))

data Heap a b =
    Heap !a b (HeapList a b)
    deriving Show

infixr 5 :-
data HeapList a b
    = Nil
    | {-# UNPACK #-} !(Heap a b) :- HeapList a b
    deriving Show

instance Foldable (Heap a) where
    foldr f b (Heap _ x xs) = f x (foldr f b xs)
    {-# INLINE foldr #-}

    foldl' f !b (Heap _ x xs) =
        case f b x of
            !b' -> foldl' f b' xs
    {-# INLINE foldl' #-}

instance Foldable (HeapList a) where
    foldr _ b Nil                 = b
    foldr f b (Heap _ x xs :- ys) = f x (foldr f (foldr f b ys) xs)
    {-# INLINABLE foldr #-}

    foldl' _ !b Nil = b
    foldl' f !b (Heap _ x xs :- ys) =
        case f b x of
            !b' -> case foldl' f b' xs of
              !b'' -> foldl' f b'' ys
    {-# INLINABLE foldl' #-}

-- |
--
-- prop> toList xs === toList (foldMap1 pure (xs :: Heap Int Int) :: NonEmpty Int)
instance Foldable1 (Heap a) where
    foldMap1 f (Heap _ x xs) = go (f x) xs
      where
        go y Nil = y
        go y (z :- zs) = y <> go (foldMap1 f z) zs
    {-# INLINE foldMap1 #-}

    toNonEmpty (Heap _ x xs) = x :| toList xs
    {-# INLINE toNonEmpty #-}

merge :: (a -> a -> Bool) -> Heap a b -> Heap a b -> Heap a b
merge cmp (Heap i x xs) (Heap j y ys)
      | cmp i j = Heap i x (Heap j y ys :- xs)
      | otherwise = Heap j y (Heap i x xs :- ys)
{-# INLINE merge #-}

instance Ord a => Semigroup (Heap a b) where
    (<>) = merge (<=)
    {-# INLINE (<>) #-}
    sconcat (ys :| [])        = ys
    sconcat (ys :| [zs])      = ys <> zs
    sconcat (ys :| (z:zz:zs)) = (ys <> z) <> sconcat (zz :| zs)
    {-# INLINABLE sconcat #-}

mergeHeaps :: (a -> a -> Bool) -> Heap a b -> HeapList a b -> Heap a b
mergeHeaps _ t Nil                 = t
mergeHeaps cmp t1 (t2 :- Nil)      = merge cmp t1 t2
mergeHeaps cmp t1 (t2 :- t3 :- ts) = merge cmp (merge cmp t1 t2) (mergeHeaps cmp t3 ts)
{-# INLINABLE mergeHeaps #-}

singleton :: a -> b -> Heap a b
singleton i x = Heap i x Nil
{-# INLINE singleton #-}

insertHeap :: (a -> a -> Bool) -> a -> b -> Heap a b -> Heap a b
insertHeap cmp !i !x (Heap j y ys)
  | cmp i j = Heap i x (Heap j y ys :- Nil)
  | otherwise = Heap j y (Heap i x Nil :- ys)
{-# INLINE insertHeap #-}

insertList :: (a -> a -> Bool) -> a -> b -> HeapList a b -> Heap a b
insertList _   !i !x Nil = Heap i x Nil
insertList cmp !i !x (y :- ys) = insertHeap cmp i x (mergeHeaps cmp y ys)
{-# INLINE insertList #-}

minView :: (a -> a -> Bool) -> Heap a b -> (a, b, Maybe (Heap a b))
minView cmp (Heap i x xs) = (i, x, case xs of
    Nil -> Nothing
    (y :- ys) -> Just (mergeHeaps cmp y ys))
{-# INLINE minView #-}

pattern MinView :: Ord a => a -> b -> Maybe (Heap a b) -> Heap a b
pattern MinView i x xs <- (minView (<=) -> (i, x, xs)) where
  MinView i x Nothing = singleton i x
  MinView i x (Just xs) = insertHeap (<=) i x xs

-- $setup
-- >>> import Test.QuickCheck
-- >>> import Control.Applicative
-- >>> import Data.Semigroup
-- >>> :{
-- instance (Arbitrary a, Arbitrary b, Ord a) => Arbitrary (Heap a b) where
--     arbitrary = fmap (sconcat . fmap (uncurry singleton)) (liftA2 (:|) arbitrary arbitrary)
-- data Tree a = Leaf a | Tree a :*: Tree a deriving (Eq, Show)
-- instance Semigroup (Tree a) where (<>) = (:*:)
-- :}
