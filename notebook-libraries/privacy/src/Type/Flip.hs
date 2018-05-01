{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeApplications #-}

module Type.Flip where

import Data.Kind
import Data.Bifunctor
import Data.Coerce
import Data.Coerce.Utilities
import Control.Lens

newtype Flip (f :: a -> b -> Type) (x :: b) (y :: a) = Flip
    { unFlip :: f y x
    }

instance Bifunctor f =>
         Bifunctor (Flip f) where
    bimap =
        (coerce :: ((a -> c) -> (b -> d) -> f b a -> f d c) -> ((a -> c) -> (b -> d) -> Flip f a b -> Flip f c d))
            (flip bimap)
    {-# INLINE bimap #-}
    first =
        (\x ->
              Flip . x . unFlip) #.
        second
    {-# INLINE first #-}
    second =
        (\x ->
              Flip . x . unFlip) #.
        first
    {-# INLINE second #-}

instance Bifunctor f => Functor (Flip f a) where
    fmap = second
    {-# INLINE fmap #-}

tflipped :: forall f1 f2 a1 a2 b1 b2. Iso (Flip f1 a1 b1) (Flip f2 a2 b2) (f1 b1 a1) (f2 b2 a2)
tflipped = coerced
{-# INLINE tflipped #-}
