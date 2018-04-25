module Numeric.Literals where

import           Prelude         hiding (fromInteger, fromRational)
import qualified Prelude         as Num

import           Data.Coerce.Utilities

import           Data.Int
import           Data.Ratio
import           Data.Word
import           Numeric.Natural

import           Data.Functor.Identity
import           Data.Functor.Const
import           Data.Semigroup
import           Data.Ord

class NaturalLiteral a where
    fromNatural :: Natural -> a

instance NaturalLiteral Int     where fromNatural = fromEnum
instance NaturalLiteral Word    where fromNatural = fromIntegral
instance NaturalLiteral Double  where fromNatural = fromIntegral
instance NaturalLiteral Float   where fromNatural = fromIntegral
instance NaturalLiteral Integer where fromNatural = fromIntegral
instance NaturalLiteral Natural where fromNatural = id
instance NaturalLiteral Int8    where fromNatural = fromIntegral
instance NaturalLiteral Int16   where fromNatural = fromIntegral
instance NaturalLiteral Int32   where fromNatural = fromIntegral
instance NaturalLiteral Int64   where fromNatural = fromIntegral
instance NaturalLiteral Word8   where fromNatural = fromIntegral
instance NaturalLiteral Word16  where fromNatural = fromIntegral
instance NaturalLiteral Word32  where fromNatural = fromIntegral
instance NaturalLiteral Word64  where fromNatural = fromIntegral

instance Integral a =>
         NaturalLiteral (Ratio a) where
    fromNatural = fromIntegral

instance NaturalLiteral a => NaturalLiteral (Identity a) where
    fromNatural = Identity #. fromNatural

instance NaturalLiteral a => NaturalLiteral (Max a) where
    fromNatural = Max #. fromNatural

instance NaturalLiteral a => NaturalLiteral (Min a) where
    fromNatural = Min #. fromNatural

instance NaturalLiteral a => NaturalLiteral (Const a b) where
    fromNatural = Const #. fromNatural

instance NaturalLiteral a => NaturalLiteral (Down a) where
    fromNatural = Down #. fromNatural

fromInteger :: NaturalLiteral a => Integer -> a
fromInteger = fromNatural . Num.fromInteger
{-# INLINE fromInteger #-}

class FractionLiteral a where
    fromRational :: Rational -> a

instance FractionLiteral Double where fromRational = Num.fromRational
instance FractionLiteral Float  where fromRational = Num.fromRational

instance Integral a =>
         FractionLiteral (Ratio a) where
    fromRational = Num.fromRational

instance FractionLiteral a => FractionLiteral (Identity a) where
    fromRational = Identity #. fromRational

instance FractionLiteral a => FractionLiteral (Max a) where
    fromRational = Max #. fromRational

instance FractionLiteral a => FractionLiteral (Min a) where
    fromRational = Min #. fromRational

instance FractionLiteral a => FractionLiteral (Const a b) where
    fromRational = Const #. fromRational

instance FractionLiteral a => FractionLiteral (Down a) where
    fromRational = Down #. fromRational
