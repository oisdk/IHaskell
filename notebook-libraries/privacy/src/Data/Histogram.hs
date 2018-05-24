module Data.Histogram where

import           Prelude                                 hiding ((+),(*))

import           Data.Map.Strict                         (Map)
import qualified Data.Map.Strict                         as Map

import           Data.Semigroup

import           Algebra.Rig
import           Algebra.Semirig

import           Data.Coerce.Utilities
import           Control.Lens

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
    (<>) = Map.unionWith (<>) `upon` getHistogram

instance (Semigroup a, Ord b) => Monoid (Histogram a b) where
    mappend = Map.unionWith (<>) `upon` getHistogram
    mempty = Histogram Map.empty

instance (Semirig a, Ord b) => Semirig (Histogram a b) where
    (+) = Map.unionWith (+) `upon` getHistogram
    (*) = Map.intersectionWith (*) `upon` getHistogram

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

instance Wrapped (Histogram a b) where
    type Unwrapped (Histogram a b) = Map b a
    _Wrapped' = coerced

instance (t ~ Map c d) => Rewrapped (Histogram a b) t

histIso :: Iso (Histogram a b) (Histogram c d) (Map b a) (Map d c)
histIso = coerced
