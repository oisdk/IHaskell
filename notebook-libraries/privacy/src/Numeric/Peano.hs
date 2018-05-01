{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE PolyKinds #-}

module Numeric.Peano where

import Data.Type.Equality
import qualified GHC.TypeLits as Lits

data Nat
    = Z
    | S Nat

data Dict c where
    Dict :: c => Dict c

witness :: (a ~ b) => a :~: b
witness = Refl

class Decidable n where
    ind :: (forall m. f m -> f ('S m)) -> f 'Z -> f n
    pre :: n ~ 'S m => Dict (Decidable m)

instance Decidable 'Z where
    ind _ = id
    {-# INLINE ind #-}
    pre = case witness of {}

instance Decidable n => Decidable ('S n) where
    ind f = f . ind f
    {-# INLINE ind #-}
    pre = Dict

type family N (n :: Lits.Nat) :: Nat where
    N 0 = 'Z
    N n = 'S (N (n Lits.- 1))
