{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}

{-# LANGUAGE TypeInType #-}
{-# LANGUAGE UndecidableInstances #-}

module Numeric.Peano
  (Nat(..)
  ,type (∝)
  ,Finite(..)
  ,type N)
  where

import qualified GHC.TypeNats as Lits
import           Data.Functor.Const
import           Data.Kind hiding (type (*))
import           Data.Constraint

data Nat
    = Z
    | S Nat

type family (t ∷ k) ∝ (n ∷ Nat) = (val ∷ Type) | val → t n k

class Finite n where
    induction ∷ t ∝ Z → (∀ k. t ∝ k → t ∝ S k) → t ∝ n

instance Finite Z where
    induction b _ = b
    {-# inline induction #-}

instance Finite n ⇒ Finite (S n) where
    induction b f = f (induction b f)
    {-# inline induction #-}

data a ↦ b
type instance ((x ∷ a) ↦ (y ∷ b)) ∝ n = (x ∝ n) → (y ∝ n)

type instance (Const a ∷ Nat → Type) ∝ n = Const a n

type instance '(Dict, c) ∝ n = Dict (c n)

type family (+) (n ∷ Nat) (m ∷ Nat) ∷ Nat where
        Z + m = m
        S n + m = S (n + m)

type family (*) (n ∷ Nat) (m ∷ Nat) ∷ Nat where
        Z * _ = Z
        S n * m = m + n * m

type family N (n ∷ Lits.Nat) ∷ Nat where
    N 0 = Z
    N n = S (N (n Lits.- 1))
