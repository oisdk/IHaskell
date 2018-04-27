module Algebra.Information.Histogram where

import           Prelude                                 hiding (Num (..))

import           Data.Map.Strict                         (Map)
import qualified Data.Map.Strict                         as Map

import           Data.Semigroup

import           Algebra.Rig
import           Algebra.Semirig

import           Data.Coerce.Utilities
import           Control.Lens                            (each, (.~), _1)

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

newtype Histogram a b = Histogram
    { getHistogram :: Map b a
    } deriving (Eq, Ord)

instance (Semigroup a, Ord b) => Semigroup (Histogram a b) where
    (<>) = Map.unionWith (<>) `ala` getHistogram

instance (Monoid a, Ord b) => Monoid (Histogram a b) where
    mappend = Map.unionWith mappend `ala` getHistogram
    mempty = Histogram Map.empty

instance (Semirig a, Ord b) => Semirig (Histogram a b) where
    (+) = Map.unionWith (+) `ala` getHistogram
    (*) = Map.intersectionWith (*) `ala` getHistogram

instance (Semirig a, Ord b) => RigZ (Histogram a b) where
    zer = Histogram Map.empty

instance (Show b, BarsPlotValue a)
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

histogramOf :: (a -> b) -> a -> Histogram b a
histogramOf f x = Histogram (Map.singleton x (f x))
