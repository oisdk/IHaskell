module Algebra.Rig where

import           Prelude hiding (Bounded(..))

import           Algebra.Semirig
import           Algebra.Bounded

import           Data.Int
import           Numeric.Natural
import           Data.Word
import           Data.Ratio

import           Data.Functor.Identity
import           Data.Functor.Const
import           Data.Ord
import           GHC.Generics
import           Data.Semigroup

import qualified Data.Set as Set
import           Data.Set (Set)

import qualified Data.Map.Strict as Map
import           Data.Map.Strict (Map)


class Semirig a => RigZ a where
    zer :: a

class Semirig a => Rig1 a where
    one :: a

type Rig a = (RigZ a, Rig1 a)

instance RigZ Bool where
    zer = False

instance RigZ Integer where zer = 0
instance RigZ Int     where zer = 0
instance RigZ Int8    where zer = 0
instance RigZ Int16   where zer = 0
instance RigZ Int32   where zer = 0
instance RigZ Int64   where zer = 0
instance RigZ Natural where zer = 0
instance RigZ Word    where zer = 0
instance RigZ Word8   where zer = 0
instance RigZ Word16  where zer = 0
instance RigZ Word32  where zer = 0
instance RigZ Word64  where zer = 0
instance RigZ Double  where zer = 0
instance RigZ Float   where zer = 0

instance Integral a =>
         RigZ (Ratio a) where
    zer = 0

deriving instance RigZ a => RigZ (Identity a)
deriving instance RigZ a => RigZ (Const a b)
deriving instance RigZ a => RigZ (Down a)

instance Rig a => RigZ [a] where
    zer = []

instance (RigZ a, RigZ b) => RigZ (a,b) where
    zer = (zer, zer)

instance (RigZ (f a), RigZ (g a)) => RigZ ((f :*: g) a) where
    zer = zer :*: zer

instance MinBound a => RigZ (Max a) where
    zer = Max minBound

instance MaxBound a => RigZ (Min a) where
    zer = Min maxBound

instance (Ord a, Semirig b) => RigZ (Map a b) where
    zer = Map.empty

instance Ord a => RigZ (Set a) where
    zer = Set.empty

instance Rig1 Bool where
    one = True

instance Rig1 Integer where one = 1
instance Rig1 Int     where one = 1
instance Rig1 Int8    where one = 1
instance Rig1 Int16   where one = 1
instance Rig1 Int32   where one = 1
instance Rig1 Int64   where one = 1
instance Rig1 Natural where one = 1
instance Rig1 Word    where one = 1
instance Rig1 Word8   where one = 1
instance Rig1 Word16  where one = 1
instance Rig1 Word32  where one = 1
instance Rig1 Word64  where one = 1
instance Rig1 Double  where one = 1
instance Rig1 Float   where one = 1

instance Integral a =>
         Rig1 (Ratio a) where
    one = 1

deriving instance Rig1 a => Rig1 (Identity a)
deriving instance Rig1 a => Rig1 (Const a b)
deriving instance Rig1 a => Rig1 (Down a)

instance Rig1 a => Rig1 [a] where
    one = [one]

instance (Rig1 a, Rig1 b) => Rig1 (a,b) where
    one = (one, one)

instance (Rig1 (f a), Rig1 (g a)) => Rig1 ((f :*: g) a) where
    one = one :*: one

instance MaxBound a => Rig1 (Max a) where
    one = Max maxBound

instance MinBound a => Rig1 (Min a) where
    one = Min minBound
