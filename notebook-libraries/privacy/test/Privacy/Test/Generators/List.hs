{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Privacy.Test.Generators.List where

import           Control.Applicative (liftA2)
import           Hedgehog              (Gen)

import           Control.Lens
import           Data.List.Indexed
import           Numeric.Peano
import           Type.Compose
import           Type.Flip

genList :: Decidable n => Gen a -> Gen (List n a)
genList gen =
    (from inductionIndex %~ ind (inductionIndex %~ liftA2 (:-) gen)) $ pure Nil
{-# INLINE genList #-}


inductionIndex
    :: forall l m a.
        Iso (Compose Gen (Flip List a) l)
            (Compose Gen (Flip List a) m)
            (Gen (List l a))
            (Gen (List m a))
inductionIndex = composed . mapping tflipped
{-# INLINE inductionIndex #-}
