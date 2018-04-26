module Algebra.Rig where

import           Algebra.Semirig

import           Data.Int
import           Numeric.Natural
import           Data.Word
import           Data.Ratio

import           Data.Functor.Identity
import           Data.Functor.Const
import           Data.Ord
import           GHC.Generics

class Semirig a => Rig a where
    one :: a

instance Rig Bool where
    one = True

instance Rig Integer where one = 1
instance Rig Int     where one = 1
instance Rig Int8    where one = 1
instance Rig Int16   where one = 1
instance Rig Int32   where one = 1
instance Rig Int64   where one = 1
instance Rig Natural where one = 1
instance Rig Word    where one = 1
instance Rig Word8   where one = 1
instance Rig Word16  where one = 1
instance Rig Word32  where one = 1
instance Rig Word64  where one = 1
instance Rig Double  where one = 1
instance Rig Float   where one = 1

instance Integral a =>
         Rig (Ratio a) where
    one = 1

instance Rig a => Rig (Identity a) where
    one = Identity one

instance Rig a => Rig (Const a b) where
    one = Const one

instance Rig a => Rig (Down a) where
    one = Down one

instance Rig a => Rig [a] where
    one = [one]

instance (Rig a, Rig b) => Rig (a,b) where
    one = (one, one)

instance (Rig (f a), Rig (g a)) => Rig ((f :*: g) a) where
    one = one :*: one
