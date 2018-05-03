{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}

{-# LANGUAGE TypeInType #-}
{-# LANGUAGE UndecidableInstances #-}

module Numeric.Peano
  (Nat(..)
  ,type (∝)
  ,ByInductionOn(..)
  ,type N)
  where

import qualified GHC.TypeNats as Lits
import           Data.Functor.Const
import           Data.Kind

data Nat
    = Z
    | S Nat

type family (t ∷ k) ∝ (n ∷ Nat) = (val ∷ Type) | val → t n k

class ByInductionOn n where
    induction ∷ (∀ k. t ∝ k → t ∝ S k) → t ∝ Z → t ∝ n

instance ByInductionOn Z where
    induction _ b = b
    {-# inline induction #-}

instance ByInductionOn n ⇒ ByInductionOn (S n) where
    induction f b = f (induction f b)
    {-# inline induction #-}

data a ↦ b
type instance ((x ∷ a) ↦ (y ∷ b)) ∝ n = (x ∝ n) → (y ∝ n)

type instance (Const a ∷ Nat → Type) ∝ n = Const a n

type family N (n ∷ Lits.Nat) ∷ Nat where
    N 0 = Z
    N n = S (N (n Lits.- 1))
