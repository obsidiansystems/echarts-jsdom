{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module ECharts.ChartOptions where

import ECharts.Types
import ECharts.Series
import Data.Default (Default, def)
import Data.Some (Some)
import GHC.Generics (Generic)
import Control.Lens

data ChartOptions = ChartOptions
  { _chartOptions_title :: Maybe Title
  , _chartOptions_legend :: Maybe Legend
  , _chartOptions_xAxis :: [Axis]
  , _chartOptions_yAxis :: [Axis]
  , _chartOptions_grid :: [Grid]
  , _chartOptions_dataZoom :: [DataZoom]
  , _chartOptions_visualMap :: [VisualMap]
  , _chartOptions_tooltip :: Maybe ToolTip
  , _chartOptions_toolbox :: Maybe ToolBox
  , _chartOptions_axisPointer :: Maybe AxisPointer
  , _chartOptions_series :: [Some SeriesT]
  }
  deriving (Generic)

instance Default ChartOptions where

makeLenses ''ChartOptions
