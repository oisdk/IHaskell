module Algebra.Information where

import           Prelude                 hiding (Num (..))

import           Algebra.Semirig
import           Algebra.Rig

import           Data.Semigroup
import           Data.Functor.Identity
import           Data.Functor.Const
import           GHC.Generics

import           Data.Coerce.Utilities


class (Ord (f a), Semirig (f a)) => Information f a where
    {-# MINIMAL information | generalize #-}
    information :: a -> f a
    information = generalize .# Identity

    generalize :: Foldable t => t a -> f a
    generalize = getGeneralization #. foldMap (Generalization #. information)

instance (Information f a, Information g a) => Information (f :*: g) a where
    information x = information x :*: information x

newtype Generalization a
    = Generalization
    { getGeneralization :: a
    } deriving (Eq, Ord, Show, Semirig)

instance Semirig a => Semigroup (Generalization a) where
    (<>) = (+)

instance Semirig a => Monoid (Generalization a) where
    mappend = (<>)
    mempty = zer

instance (Ord a, Rig a) => Information (Const a) b where
    information = const one
