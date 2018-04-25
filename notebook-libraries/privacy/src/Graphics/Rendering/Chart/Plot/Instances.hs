{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}

module Graphics.Rendering.Chart.Plot.Instances where

import           Data.Functor.Const
import           Graphics.Rendering.Chart          (BarsPlotValue (..),
                                                    PlotValue (..))
import           Graphics.Rendering.Chart.Axis.Int

import           Numeric.Natural

deriving instance PlotValue a => PlotValue (Const a b)
deriving instance BarsPlotValue a => BarsPlotValue (Const a b)

instance PlotValue Natural where
    toValue    = fromIntegral
    fromValue  = round
    autoAxis   = autoScaledIntAxis defaultIntAxis

instance BarsPlotValue Natural where
    barsReference = 0
    barsAdd       = (+)
