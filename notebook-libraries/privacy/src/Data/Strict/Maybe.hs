{-# options_ghc -fno-warn-orphans #-}

module Data.Strict.Maybe where

import Control.Lens

instance Strict (Maybe a) (Maybe' a) where
    strict = iso (maybe Nothing' Just') (maybe' Nothing Just)
    {-# inline strict #-}

maybe' :: p -> (t -> p) -> Maybe' t -> p
maybe' b _ Nothing' = b
maybe' _ f (Just' x) = f x
{-# inline maybe' #-}

data Maybe' a
    = Nothing'
    | Just' !a
    deriving (Functor, Foldable, Traversable, Show, Eq, Ord)

instance Applicative Maybe' where
    pure = Just'
    Nothing' <*> _ = Nothing'
    _ <*> Nothing' = Nothing'
    Just' f <*> Just' x = Just' (f x)

instance Monad Maybe' where
    Nothing' >>= _ = Nothing'
    Just' x >>= f = f x
