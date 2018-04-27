module Algebra.Semirig where

import           Prelude hiding (Num (..))
import qualified Prelude as Num

import           Numeric.Natural
import           Data.Ratio
import           Data.Int
import           Data.Word

import           Data.Functor.Const
import           Data.Functor.Identity
import           Data.Ord
import           GHC.Generics

import           Data.Semigroup.Foldable
import           Data.Semigroup

import           Data.Coerce.Utilities

import qualified Data.Set as Set
import           Data.Set (Set)

import qualified Data.Map.Strict as Map
import           Data.Map.Strict (Map)

class Semirig a where
    infixl 6 +
    (+) :: a -> a -> a
    infixl 7 *
    (*) :: a -> a -> a

instance Semirig Bool where
    (+) = (||)
    (*) = (&&)

instance Semirig Int where
    (+) = (Num.+)
    (*) = (Num.*)

instance Semirig Integer where
    (+) = (Num.+)
    (*) = (Num.*)

instance Semirig Natural where
    (+) = (Num.+)
    (*) = (Num.*)

instance Semirig Word where
    (+) = (Num.+)
    (*) = (Num.*)

instance Semirig Double where
    (+) = (Num.+)
    (*) = (Num.*)

instance Semirig Float where
    (+) = (Num.+)
    (*) = (Num.*)

instance Integral a => Semirig (Ratio a) where
    (+) = (Num.+)
    (*) = (Num.*)

instance Semirig Int8 where
    (+) = (Num.+)
    (*) = (Num.*)

instance Semirig Int16 where
    (+) = (Num.+)
    (*) = (Num.*)

instance Semirig Int32 where
    (+) = (Num.+)
    (*) = (Num.*)

instance Semirig Int64 where
    (+) = (Num.+)
    (*) = (Num.*)

instance Semirig Word8 where
    (+) = (Num.+)
    (*) = (Num.*)

instance Semirig Word16 where
    (+) = (Num.+)
    (*) = (Num.*)

instance Semirig Word32 where
    (+) = (Num.+)
    (*) = (Num.*)

instance Semirig Word64 where
    (+) = (Num.+)
    (*) = (Num.*)

deriving instance Semirig a => Semirig (Const a b)
deriving instance Semirig a => Semirig (Identity a)
deriving instance Semirig a => Semirig (Down a)

instance Semirig a =>
         Semirig [a] where
    [] + ys = ys
    xs + [] = xs
    (x:xs) + (y:ys) = x + y : xs + ys
    _ * [] = []
    xs * (y':ys) = foldr f [] xs
      where
        f x zs = x * y' : foldr (g x) id ys zs
        g x y a (z:zs) = x * y + z : a zs
        g x y a [] = x * y : a []

instance (Semirig a, Semirig b) => Semirig (a,b) where
    (xl,yl) + (xr,yr) = (xl+xr,yl+yr)
    (xl,yl) * (xr,yr) = (xl*xr,yl*yr)

instance (Semirig (f a), Semirig (g a)) => Semirig ((f :*: g) a) where
    (xl :*: yl) + (xr :*: yr) = (xl+xr) :*: (yl+yr)
    (xl :*: yl) * (xr :*: yr) = (xl*xr) :*: (yl*yr)

instance Ord a => Semirig (Max a) where
    (+) = (<>)
    (*) = min

instance Ord a => Semirig (Min a) where
    (+) = (<>)
    (*) = max

instance Ord a => Semirig (Set a) where
    (+) = Set.union
    (*) = Set.intersection

instance (Ord a, Semirig b) => Semirig (Map a b) where
    (+) = Map.unionWith (+)
    (*) = Map.intersectionWith (*)

newtype Generalization a = Generalization
    { getGeneralization :: a
    } deriving (Eq, Ord, Show)

instance Semirig a => Semigroup (Generalization a) where
    (<>) = (+) `ala` getGeneralization

generalize :: (Foldable1 f, Semirig a) => f a -> a
generalize = getGeneralization #. foldMap1 Generalization

generalizeTo :: (Foldable1 f, Semirig b) => (a -> b) -> f a -> b
generalizeTo f = getGeneralization #. foldMap1 (Generalization #. f)

newtype Refinement a = Refinement
    { getRefinement :: a
    }

instance Semirig a => Semigroup (Refinement a) where
    (<>) = (*) `ala` getRefinement

refine :: (Foldable1 f, Semirig a) => f a -> a
refine = getRefinement #. foldMap1 Refinement

refineTo :: (Foldable1 f, Semirig b) => (a -> b) -> f a -> b
refineTo f = getRefinement #. foldMap1 (Refinement #. f)
