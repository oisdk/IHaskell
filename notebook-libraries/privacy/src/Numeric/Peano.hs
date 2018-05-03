{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}

{-# LANGUAGE TypeInType #-}
{-# LANGUAGE UndecidableInstances #-}

module Numeric.Peano where

import qualified GHC.TypeNats as Lits

import Data.Kind

data Nat = Z | S Nat

type family (t :: k) `OfSize` (n :: Nat) = (val :: Type) | val -> t n k

class ByInductionOn n where
  induction :: (∀ k. t `OfSize` k -> t `OfSize` S k) -> t `OfSize` Z -> t `OfSize` n
  deduction :: (∀ k. t `OfSize` S k -> t `OfSize` k) -> t `OfSize` n -> t `OfSize` Z

instance ByInductionOn Z where
  induction _ b = b
  {-# INLINE induction #-}
  deduction _ b = b
  {-# INLINE deduction #-}

instance ByInductionOn n => ByInductionOn (S n) where
  induction f b = f (induction f b)
  {-# INLINE induction #-}
  deduction f b = deduction f (f b)
  {-# INLINE deduction #-}

data a :-> b

type instance ((a :: k1) :-> (b :: k2)) `OfSize` n = (a `OfSize` n) -> (b `OfSize` n)

type family N (n :: Lits.Nat) :: Nat where
    N 0 = Z
    N n = S (N (n Lits.- 1))
