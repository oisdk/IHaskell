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

class Rig a => Ring a where
    {-# MINIMAL negate | (-) #-}
    negate :: a -> a
    negate = (-) zer

    infixl 6 -
    (-) :: a -> a -> a
    (-) x = (+) x . negate

instance Ring Int where
    negate = Num.negate
    (-) = (Num.-)

instance Ring Integer where
    negate = Num.negate
    (-) = (Num.-)

instance Ring Int8 where
    negate = Num.negate
    (-) = (Num.-)

instance Ring Int16 where
    negate = Num.negate
    (-) = (Num.-)

instance Ring Int32 where
    negate = Num.negate
    (-) = (Num.-)

instance Ring Int64 where
    negate = Num.negate
    (-) = (Num.-)

instance Ring Double where
    negate = Num.negate
    (-) = (Num.-)

instance Ring Float where
    negate = Num.negate
    (-) = (Num.-)

instance Integral a => Ring (Ratio a) where
    negate = Num.negate
    (-) = (Num.-)

deriving instance Ring a => Ring (Identity a)
deriving instance Ring a => Ring (Const a b)
deriving instance Ring a => Ring (Down a)

instance Ring a =>
         Ring [a] where
    negate = map negate
    [] - ys = map negate ys
    xs - [] = xs
    (x:xs) - (y:ys) = x - y : xs - ys

instance (Ring a, Ring b) => Ring (a,b) where
    negate (x,y) = (negate x, negate y)
    (xl,yl) - (xr,yr) = (xl-xr,yl-yr)

instance (Ring (f a), Ring (g a)) => Ring ((f :*: g) a) where
    negate (x :*: y) = negate x :*: negate y
    (xl :*: yl) - (xr :*: yr) = (xl-xr) :*: (yl-yr)
