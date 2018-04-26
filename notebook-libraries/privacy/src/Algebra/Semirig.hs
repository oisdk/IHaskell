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

class Semirig a where
    zer :: a
    infixl 6 +
    (+) :: a -> a -> a
    infixl 7 *
    (*) :: a -> a -> a

instance Semirig Bool where
    zer = False
    (+) = (||)
    (*) = (&&)

instance Semirig Int where
    zer = 0
    (+) = (Num.+)
    (*) = (Num.*)

instance Semirig Integer where
    zer = 0
    (+) = (Num.+)
    (*) = (Num.*)

instance Semirig Natural where
    zer = 0
    (+) = (Num.+)
    (*) = (Num.*)

instance Semirig Word where
    zer = 0
    (+) = (Num.+)
    (*) = (Num.*)

instance Semirig Double where
    zer = 0
    (+) = (Num.+)
    (*) = (Num.*)

instance Semirig Float where
    zer = 0
    (+) = (Num.+)
    (*) = (Num.*)

instance Integral a => Semirig (Ratio a) where
    zer = 0
    (+) = (Num.+)
    (*) = (Num.*)

instance Semirig Int8 where
    zer = 0
    (+) = (Num.+)
    (*) = (Num.*)

instance Semirig Int16 where
    zer = 0
    (+) = (Num.+)
    (*) = (Num.*)

instance Semirig Int32 where
    zer = 0
    (+) = (Num.+)
    (*) = (Num.*)

instance Semirig Int64 where
    zer = 0
    (+) = (Num.+)
    (*) = (Num.*)

instance Semirig Word8 where
    zer = 0
    (+) = (Num.+)
    (*) = (Num.*)

instance Semirig Word16 where
    zer = 0
    (+) = (Num.+)
    (*) = (Num.*)

instance Semirig Word32 where
    zer = 0
    (+) = (Num.+)
    (*) = (Num.*)

instance Semirig Word64 where
    zer = 0
    (+) = (Num.+)
    (*) = (Num.*)

deriving instance Semirig a => Semirig (Const a b)
deriving instance Semirig a => Semirig (Identity a)
deriving instance Semirig a => Semirig (Down a)

instance Semirig a =>
         Semirig [a] where
    zer = []
    [] + ys = ys
    xs + [] = xs
    (x:xs) + (y:ys) = x + y : xs + ys
    _ * [] = []
    xs * ys = foldr f [] xs
      where
        f x zs = foldr (g x) id ys (zer : zs)
        g x y a (z:zs) = x * y + z : a zs
        g x y a [] = x * y : a []

instance (Semirig a, Semirig b) => Semirig (a,b) where
    zer = (zer,zer)
    (xl,yl) + (xr,yr) = (xl+xr,yl+yr)
    (xl,yl) * (xr,yr) = (xl*xr,yl*yr)

instance (Semirig (f a), Semirig (g a)) => Semirig ((f :*: g) a) where
    zer = zer :*: zer
    (xl :*: yl) + (xr :*: yr) = (xl+xr) :*: (yl+yr)
    (xl :*: yl) * (xr :*: yr) = (xl*xr) :*: (yl*yr)

