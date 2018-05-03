{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE TypeApplications     #-}

{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

module Data.List.NonEmpty.Static
  (nonEmpty)
  where

import qualified Data.List.NonEmpty as List
import GHC.TypeNats
import GHC.TypeLits hiding (natVal, KnownNat)
import Numeric.Natural

class NonEmpty (xs :: [Nat]) where
    nonEmpty :: List.NonEmpty Natural

instance (KnownNat x, KnownList xs) => NonEmpty (x ': xs) where
    nonEmpty = natVal (undefined :: proxy x) List.:| toList @ xs

instance (TypeError ('Text "List is not empty")) => NonEmpty '[] where
    nonEmpty = error "Data.List.NonEmpty.Static: bug!"

class KnownList (xs :: [Nat]) where
    toList :: [Natural]

instance KnownList '[] where
    toList = []

instance (KnownList xs, KnownNat x) => KnownList (x ': xs) where
    toList = natVal (undefined :: proxy x) : toList @ xs
