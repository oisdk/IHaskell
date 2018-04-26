{-# LANGUAGE OverloadedStrings #-}

module Algebra.Information.Huffman where

import           Data.Bool              (bool)
import           Data.Coerce.Utilities

import           IHaskell.Display       (IHaskellDisplay (display))
import           IHaskell.Display.Blaze ()

import qualified Text.Blaze.Html5       as Blaze

newtype Code = Code { getCode :: [Bool] }

instance Blaze.ToMarkup Code where
    toMarkup = Blaze.pre . Blaze.toMarkup . map (bool '0' '1') .# getCode

instance Show Code where
    showsPrec _ = flip (foldr ((:) . bool '0' '1')) .# getCode

instance IHaskellDisplay Code where
    display = display . Blaze.toMarkup
