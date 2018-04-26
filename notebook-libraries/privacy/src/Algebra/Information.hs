{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies               #-}

module Algebra.Information where

import           Prelude                 hiding (Num (..))

import           Algebra.Semirig
import           Algebra.Rig

import           Data.Semigroup
import           Data.Functor.Identity
import           Data.Functor.Const
import           Data.Ord

import           Data.Coerce.Utilities

type family Domain (information :: *) :: *

type instance Domain (a,b) = (Domain a, Domain b)
type instance Domain (Const a b) = b
type instance Domain (Identity a) = Domain a
type instance Domain (Down a) = Domain a

class (Ord a, Semirig a) => Information a where
    {-# MINIMAL information | generalize #-}
    information :: Domain a -> a
    information = generalize .# Identity

    generalize :: Foldable f => f (Domain a) -> a
    generalize = getGeneralization #. foldMap (Generalization #. information)

instance (Information a, Information b) => Information (a,b) where
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
    information = const one

instance Information a => Information (Identity a) where
    information = Identity #. information

instance Information a => Information (Down a) where
    information = Down #. information
