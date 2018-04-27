{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes  #-}

{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

module Data.String.NonEmpty
  (nonEmptyString)
  where

import qualified Data.List.NonEmpty as List
import GHC.TypeLits
import GHC.Exts

type family NonEmpty (n :: Symbol) :: Constraint where
    NonEmpty "" = TypeError ('Text "String isn't empty")
    NonEmpty _ = ()

nonEmptyString :: forall n. (NonEmpty n, KnownSymbol n) => List.NonEmpty Char
nonEmptyString = case symbolVal (undefined :: proxy n) of
  [] -> error "Data.String.NonEmpty: bug!"
  (x:xs) -> x List.:| xs
