{-# options_ghc -fno-warn-orphans #-}

module Data.Strict.Pair where

import Data.Semigroup
import Control.Lens

data a :!: b = !a :!: !b deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

instance (Semigroup a, Semigroup b) => Semigroup (a :!: b) where
    (lx :!: ly) <> (rx :!: ry) = (lx <> rx) :!: (ly <> ry)

instance (Monoid a, Monoid b) => Monoid (a :!: b) where
    (lx :!: ly) `mappend` (rx :!: ry) = (lx `mappend` rx) :!: (ly `mappend` ry)
    mempty = mempty :!: mempty

curry' :: ((a :!: b) -> c) -> a -> b -> c
curry' f x y = f (x :!: y)

uncurry' :: (a -> b -> c) -> (a :!: b) -> c
uncurry' f (x :!: y) = f x y

instance Monoid a =>
         Applicative ((:!:) a) where
    pure = (:!:) mempty
    (ft :!: f) <*> (xt :!: x) = mappend ft xt :!: f x

instance Monoid a =>
         Monad ((:!:) a) where
    (xt :!: x) >>= f =
        case f x of
            yt :!: y -> mappend xt yt :!: y

instance Strict (a,b) (a :!: b) where
    strict = iso (uncurry (:!:)) (uncurry' (,))
