{-# LANGUAGE RankNTypes #-}

module Graphics.Rendering.Chart.Axis.Iso where

import           Control.Lens             (Iso', dimapping, firsting, from, iso,
                                           lmapping, mapping, rmapping, (^.))
import           Graphics.Rendering.Chart (AxisData (..), AxisFn)

axisData :: Iso' a b -> Iso' (AxisData a) (AxisData b)
axisData i' = iso (toData i') (toData (from i'))
  where
    toData l (AxisData vs vp tv tc lb gd) =
        AxisData
            vs
            (vp ^. (rmapping . lmapping . from) l)
            (tv ^. (rmapping . rmapping) l)
            (tc ^. (mapping . firsting) l)
            (lb ^. (mapping . mapping . firsting) l)
            (gd ^. mapping l)


axisFn :: Iso' a b -> Iso' (AxisFn a) (AxisFn b)
axisFn i = dimapping (mapping (from i)) (axisData i)
