{-# LANGUAGE PolyKinds #-}

module Type.Compose where

import Data.Kind
import Control.Lens
import Data.Coerce.Utilities
import Control.Applicative
import Data.Coerce

newtype Compose (f :: b -> Type) (g :: a -> b) (x :: a) = Compose
    { getCompose :: f (g x)
    }

composed :: forall f1 f2 g1 g2 x1 x2. Iso (Compose f1 g1 x1) (Compose f2 g2 x2) (f1 (g1 x1)) (f2 (g2 x2))
composed = coerced
{-# INLINE composed #-}

instance (Functor f, Functor g) => Functor (Compose f g) where
    fmap = over composed #. fmap . fmap
    {-# INLINE fmap #-}
    (<$) = over composed #. fmap . (<$)
    {-# INLINE (<$) #-}

instance (Applicative f, Applicative g) =>
         Applicative (Compose f g) where
    pure = Compose #. pure . pure
    {-# INLINE pure #-}
    (<*>) =
        (coerce :: (f (g (a -> b)) -> f (g a) -> f (g b)) -> Compose f g (a -> b) -> Compose f g a -> Compose f g b)
            (liftA2 (<*>))
    {-# INLINE (<*>) #-}
    liftA2 =
        (coerce :: ((a -> b -> c) -> f (g a) -> f (g b) -> f (g c)) -> ((a -> b -> c) -> Compose f g a -> Compose f g b -> Compose f g c))
            (liftA2 . liftA2)
    {-# INLINE liftA2 #-}

instance (Foldable f, Foldable g) => Foldable (Compose f g) where
    foldMap = views composed #. foldMap . foldMap
    {-# INLINE foldMap #-}
    foldr = (views composed .) #. foldr . flip . foldr
    {-# INLINE foldr #-}
    foldl = (views composed .) #. foldl . foldl
    {-# INLINE foldl #-}
