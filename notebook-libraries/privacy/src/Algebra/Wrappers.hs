module Algebra.Wrappers where

import Prelude hiding (Num(..))

import Data.Semigroup
import Data.Semigroup.Foldable
import Algebra.Semirig
import Algebra.Rig
import Control.Lens
import Data.Coerce.Utilities

newtype Generalization a = Generalization
    { getGeneralization :: a
    } deriving (Eq, Ord, Show)

instance Semirig a => Semigroup (Generalization a) where
    (<>) = (+) `upon` getGeneralization

instance Wrapped (Generalization a) where
    type Unwrapped (Generalization a) = a
    _Wrapped' = coerced

instance (t ~ Generalization b) => Rewrapped (Generalization a) t

generalize :: (Foldable1 f, Semigroup a) => f a -> a
generalize = fold1

generalizeTo :: (Foldable1 f, Semigroup b) => (a -> b) -> f a -> b
generalizeTo = foldMap1

instance RigZ a => Monoid (Generalization a) where
    mempty = Generalization zer
    mappend = (<>)

newtype Refinement a = Refinement
    { getRefinement :: a
    }

instance Semirig a => Semigroup (Refinement a) where
    (<>) = (*) `upon` getRefinement

refine :: (Foldable1 f, Semirig a) => f a -> a
refine = getRefinement #. foldMap1 Refinement

refineTo :: (Foldable1 f, Semirig b) => (a -> b) -> f a -> b
refineTo f = getRefinement #. foldMap1 (Refinement #. f)

instance Wrapped (Refinement a) where
    type Unwrapped (Refinement a) = a
    _Wrapped' = coerced

instance (t ~ Refinement b) => Rewrapped (Refinement a) t

instance Rig1 a => Monoid (Refinement a) where
    mempty = Refinement one
    mappend = (<>)
