{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies               #-}

module Algebra.Information where

import           Prelude                 hiding (Num (..))

import           Algebra.Semirig
import           Algebra.Rig

import           Data.Semigroup
import           Data.Functor.Identity
import           Data.Functor.Const

import           Data.Coerce.Utilities

class (Ord a, Semirig a) => Information a where
    type Domain a
    {-# MINIMAL information | generalize #-}
    information :: Domain a -> a
    information = generalize .# Identity

    generalize :: Foldable f => f (Domain a) -> a
    generalize = getGeneralization #. foldMap (Generalization #. information)

instance (Information a, Information b) => Information (a,b) where
    type Domain (a,b) = (Domain a, Domain b)
    information (x,y) = (information x, information y)

newtype Generalization a
    = Generalization
    { getGeneralization :: a
    } deriving (Eq, Ord, Show, Semirig)

instance Semirig a => Semigroup (Generalization a) where
    (<>) = (+)

instance Semirig a => Monoid (Generalization a) where
    mappend = (<>)
    mempty = zer

instance (Ord a, Rig a) => Information (Const a b) where
    type Domain (Const a b) = b
    information = const one
