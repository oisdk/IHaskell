module Algebra.Ring where

import           Algebra.Rig
import           Algebra.Semirig

import           Prelude               hiding (Num (..))
import qualified Prelude               as Num

import           Data.Int
import           Data.Ratio

import           Data.Functor.Const
import           Data.Functor.Identity
import           Data.Ord
import           GHC.Generics

class Semirig a => Semiring a where
    {-# MINIMAL negate #-}
    negate :: a -> a

    infixl 6 -
    (-) :: a -> a -> a
    (-) x = (+) x . negate

instance Semiring Int where
    negate = Num.negate
    (-) = (Num.-)

instance Semiring Integer where
    negate = Num.negate
    (-) = (Num.-)

instance Semiring Int8 where
    negate = Num.negate
    (-) = (Num.-)

instance Semiring Int16 where
    negate = Num.negate
    (-) = (Num.-)

instance Semiring Int32 where
    negate = Num.negate
    (-) = (Num.-)

instance Semiring Int64 where
    negate = Num.negate
    (-) = (Num.-)

instance Semiring Double where
    negate = Num.negate
    (-) = (Num.-)

instance Semiring Float where
    negate = Num.negate
    (-) = (Num.-)

instance Integral a => Semiring (Ratio a) where
    negate = Num.negate
    (-) = (Num.-)

deriving instance Semiring a => Semiring (Identity a)
deriving instance Semiring a => Semiring (Const a b)
deriving instance Semiring a => Semiring (Down a)

instance Semiring a =>
         Semiring [a] where
    negate = map negate
    [] - ys = map negate ys
    xs - [] = xs
    (x:xs) - (y:ys) = x - y : xs - ys

instance (Semiring a, Semiring b) => Semiring (a,b) where
    negate (x,y) = (negate x, negate y)
    (xl,yl) - (xr,yr) = (xl-xr,yl-yr)

instance (Semiring (f a), Semiring (g a)) => Semiring ((f :*: g) a) where
    negate (x :*: y) = negate x :*: negate y
    (xl :*: yl) - (xr :*: yr) = (xl-xr) :*: (yl-yr)

type RingZ a = (Semiring a, RigZ a)
type Ring1 a = (Semiring a, Rig1 a)
type Ring a = (Semiring a, RigZ a, Rig1 a)
