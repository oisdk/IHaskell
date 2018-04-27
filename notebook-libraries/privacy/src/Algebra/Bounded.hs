module Algebra.Bounded where

import           Prelude               hiding (Bounded (..))
import qualified Prelude

import           Data.Ord
import           Numeric.Natural

import           Data.Functor.Const
import           Data.Functor.Identity
import           Data.Int
import           Data.Semigroup
import           Data.Word

class Ord a => MinBound a where minBound :: a
class Ord a => MaxBound a where maxBound :: a

instance MinBound a => MaxBound (Down a) where
    maxBound = Down minBound

instance MaxBound a => MinBound (Down a) where
    minBound = Down maxBound

instance Ord a => MinBound (Maybe a) where
    minBound = Nothing

instance MinBound Bool where minBound = False
instance MaxBound Bool where maxBound = True

instance MinBound Natural where minBound = 0

instance MinBound Int   where minBound = Prelude.minBound
instance MaxBound Int   where maxBound = Prelude.maxBound
instance MinBound Int8  where minBound = Prelude.minBound
instance MaxBound Int8  where maxBound = Prelude.maxBound
instance MinBound Int16 where minBound = Prelude.minBound
instance MaxBound Int16 where maxBound = Prelude.maxBound
instance MinBound Int32 where minBound = Prelude.minBound
instance MaxBound Int32 where maxBound = Prelude.maxBound
instance MinBound Int64 where minBound = Prelude.minBound
instance MaxBound Int64 where maxBound = Prelude.maxBound

instance MinBound Word   where minBound = Prelude.minBound
instance MaxBound Word   where maxBound = Prelude.maxBound
instance MinBound Word8  where minBound = Prelude.minBound
instance MaxBound Word8  where maxBound = Prelude.maxBound
instance MinBound Word16 where minBound = Prelude.minBound
instance MaxBound Word16 where maxBound = Prelude.maxBound
instance MinBound Word32 where minBound = Prelude.minBound
instance MaxBound Word32 where maxBound = Prelude.maxBound
instance MinBound Word64 where minBound = Prelude.minBound
instance MaxBound Word64 where maxBound = Prelude.maxBound

instance (MinBound a, MinBound b) => MinBound (a,b) where
    minBound = (minBound,minBound)

instance (MaxBound a, MaxBound b) => MaxBound (a,b) where
    maxBound = (maxBound,maxBound)

deriving instance MinBound a => MinBound (Identity a)
deriving instance MaxBound a => MaxBound (Identity a)
deriving instance MinBound a => MinBound (Max a)
deriving instance MaxBound a => MaxBound (Max a)
deriving instance MinBound a => MinBound (Min a)
deriving instance MaxBound a => MaxBound (Min a)
deriving instance MinBound a => MinBound (Const a b)
deriving instance MaxBound a => MaxBound (Const a b)

instance Ord a => MinBound [a] where
    minBound = []

instance MaxBound a => MaxBound [a] where
    maxBound = repeat maxBound
