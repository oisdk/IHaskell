module Data.Interval where

import Control.Lens
import IHaskell.Display
import Text.LaTeX.Base.Texy
import Text.LaTeX.Packages.AMSFonts
import GHC.Exts (fromString)
import Text.LaTeX.Base.Class
import Text.LaTeX.Base.Render
import Data.Text (unpack)
import Text.LaTeX.Base.Syntax

data Interval a =
    Interval (Maybe a)
             (Maybe a)
    deriving (Eq,Ord,Show)

lowerBound :: Lens' (Interval a) (Maybe a)
lowerBound f (Interval lb ub) = fmap (flip Interval ub) (f lb)

upperBound :: Lens' (Interval a) (Maybe a)
upperBound f (Interval lb ub) = fmap (Interval lb) (f ub)

instance Texy a =>
         Texy (Interval a) where
    texy (Interval Nothing Nothing) = mathbb (fromString "U")
    texy (Interval (Just lb) Nothing) = texy lb `mappend` comm0 "leq"
    texy (Interval Nothing (Just ub)) = fromString "<" `mappend` texy ub
    texy (Interval (Just lb) (Just ub)) =
        commS "mathopen[" `mappend` texy lb `mappend` fromString "," `mappend`
        texy ub `mappend`
        commS "mathclose)"

instance Texy a =>
         IHaskellDisplay (Interval a) where
    display interval =
        pure (Display [latex (unpack (render (texy interval :: LaTeX)))])

