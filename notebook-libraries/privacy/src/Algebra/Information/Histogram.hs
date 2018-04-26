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

newtype Histogram f a
    = Histogram
    { getMeasures :: Map a (f a)
    } deriving (Eq, Ord)

instance (Semirig (f a), Ord a)
        => Semirig (Histogram f a) where
    (+) = Map.unionWith (+) `ala` getMeasures
    (*) = Map.intersectionWith (*) `ala` getMeasures
    zer = Histogram Map.empty

instance (Information f a, Ord a) =>
         Information (Histogram f) a where
    information x = Histogram (Map.singleton x (information x))

instance (Show a, BarsPlotValue (f a))
        => IHaskellDisplay (Histogram f a) where
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

(!) :: (Semirig (f a), Ord a) => Histogram f a -> a -> f a
(!) (Histogram xs) y = Map.findWithDefault zer y xs
