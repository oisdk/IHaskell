module Algebra.Information.Histogram where

import           Prelude                                 hiding ((+),(*))
import qualified Prelude

import           Data.Map.Strict                         (Map)
import qualified Data.Map.Strict                         as Map

import           Data.Semigroup

import           Algebra.Rig
import           Algebra.Semirig

import           Data.Strict.Pair
import           Data.Coerce.Utilities
import           Control.Lens hiding (ala)
import           Data.Map.Lens
import           GHC.Base                                (oneShot)

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

instance (Semigroup a, Ord b) => Monoid (Histogram a b) where
    mappend = Map.unionWith (<>) `ala` getHistogram
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

histogramOf :: IndexedGetting b (Map b a) s a -> s -> Histogram a b
histogramOf x y = Histogram (toMapOf x y)

median :: (Integral b, Fractional a) => Histogram b a -> a
median (Histogram xs) =
    Map.foldrWithKey
        f
        (error "Algebra.Information.Histogram.median: empty histogram")
        xs
        m
  where
    s = sum xs
    m = s `div` 2
    f k v a =
        oneShot
            (\ !n ->
                  let nv = n - v
                  in case compare nv 0 of
                         LT -> k
                         EQ | even s -> (k Prelude.+ a (-1)) / 2
                         _ -> a nv)

averageOf :: (Fractional a) => Getting (Sum a :!: Sum a) s (a,a) -> s -> a
averageOf ln x =
    uncurry'
        (\(Sum n) (Sum d) ->
              n / d) $
    foldMapOf
        ln
        (\(n,d) ->
              Sum (n Prelude.* d) :!: Sum d)
        x

average :: (Integral b, Fractional a) => Histogram b a -> a
average = averageOf (ifolded.withIndex.to (fmap fromIntegral)) .# getHistogram

mapHist :: (Semigroup m, Ord b) => (a -> b) -> Histogram m a -> Histogram m b
mapHist f (Histogram xs) = Histogram (Map.mapKeysWith (<>) f xs)

instance Wrapped (Histogram a b) where
    type Unwrapped (Histogram a b) = Map b a
    _Wrapped' = coerced

instance Rewrapped (Histogram a b) (Map b a)

histIso :: Iso (Histogram a b) (Histogram c d) (Map b a) (Map d c)
histIso = coerced
