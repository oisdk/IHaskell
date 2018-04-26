{-# LANGUAGE RankNTypes   #-}
{-# LANGUAGE TypeFamilies #-}

module Algebra.Information.Histogram where

import           Prelude                                 hiding (Num (..))

import           Data.Map.Strict                         (Map)
import qualified Data.Map.Strict                         as Map

import           Algebra.Information
import           Algebra.Semirig

import           Control.Lens                            (each, (.~), _1)

import           Data.Coerce.Utilities
import           Data.Colour                             (withOpacity)
import           Data.Colour.Names                       (cornflowerblue)
import           Data.Default.Class                      (def)
import           Graphics.Rendering.Chart                (BarsPlotValue (..),
                                                          addIndexes,
                                                          autoIndexAxis,
                                                          fill_color,
                                                          laxis_generate,
                                                          laxis_title,
                                                          layout_plots,
                                                          layout_x_axis,
                                                          layout_y_axis,
                                                          plotBars,
                                                         plot_bars_item_styles,
                                                          plot_bars_values,
                                                          toRenderable)
import           IHaskell.Display                        (IHaskellDisplay (display))
import           IHaskell.Display.Charts                 ()

import           Graphics.Rendering.Chart.Plot.Instances ()

newtype Histogram a b
    = Histogram
    { getMeasures :: Map b a
    } deriving (Eq, Ord)

instance (Semirig a, Ord b)
        => Semirig (Histogram a b) where
    (+) = Map.unionWith (+) `ala` getMeasures
    (*) = Map.intersectionWith (*) `ala` getMeasures
    zer = Histogram Map.empty

type instance Domain (Histogram a b) = b

instance (Information a, Ord b, Domain a ~ b) =>
         Information (Histogram a b) where
    information x = Histogram (Map.singleton x (information x))

instance Integral a => Foldable (Histogram a) where
    foldMap f (Histogram xs) =
        Map.foldMapWithKey (rep . f) xs
      where
        rep x 1 = x
        rep x n
          | even n = mappend y y
          | otherwise = mappend x (mappend y y)
          where y = rep x (n `div` 2)

instance (Show b, Ord a, BarsPlotValue a)
        => IHaskellDisplay (Histogram a b) where
    display (Histogram freqs)
        = display
        $ toRenderable
        $ layout_x_axis.laxis_generate .~ autoIndexAxis keys
        $ layout_y_axis.laxis_title .~ "Measure"
        $ layout_plots .~ [ plotBars
                          $ plot_bars_values .~ addIndexes vals
                          $ plot_bars_item_styles.each._1.fill_color .~
                                cornflowerblue `withOpacity` 0.3
                          $ def ]
        $ def
      where
        (keys,vals) = unzip [ (show k, [v]) | (k,v) <- Map.toList freqs ]

(!) :: (Semirig a, Ord b) => Histogram a b -> b -> a
(!) (Histogram xs) y = Map.findWithDefault zer y xs
