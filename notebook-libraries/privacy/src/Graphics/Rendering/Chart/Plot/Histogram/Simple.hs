module Graphics.Rendering.Chart.Plot.Histogram.Simple where

import           Control.Lens                            ((.~))
import           Data.Colour                             (withOpacity)
import           Data.Colour.Names                       (cornflowerblue)
import           Data.Default.Class                      (def)
import           Graphics.Rendering.Chart                (Layout, PlotValue,
                                                          Renderable,
                                                          fill_color,
                                                          layout_plots,
                                                          toRenderable)
import           Graphics.Rendering.Chart.Plot.Histogram (histToPlot,
                                                          plot_hist_bins,
                                                          plot_hist_fill_style,
                                                          plot_hist_range,
                                                          plot_hist_values)

contHist
    :: (RealFrac a, PlotValue a)
    => a -> a -> [a] -> Renderable ()
contHist binSize lowerBound values =
    toRenderable
  $ layout_plots .~ [ histToPlot
                    $ plot_hist_values .~ values
                    $ plot_hist_bins .~ bins
                    $ plot_hist_range .~
                        Just (lowerBound
                             ,lowerBound + fromIntegral bins * binSize)
                    $ plot_hist_fill_style . fill_color .~
                        cornflowerblue `withOpacity` 0.3
                    $ def ]
  $ (def :: PlotValue a => Layout a Int)
  where
    bins = ceiling ((maximum values - lowerBound) / binSize)
