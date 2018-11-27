module ECharts.ChartOptions where

import ECharts.Types
import ECharts.Series
import Data.Default (Default, def)
import Data.Some (Some)

data ChartOptions = ChartOptions
  { _chartOptions_title :: Title
  , _chartOptions_legend :: Legend
  , _chartOptions_xAxis :: [Axis]
  , _chartOptions_yAxis :: [Axis]
  , _chartOptions_grid :: Grid
  , _chartOptions_dataZoom :: [DataZoom]
  , _chartOptions_tooltip :: ToolTip
  , _chartOptions_toolbox :: ToolBox
  , _chartOptions_series :: [Some SeriesT]
  }

instance Default ChartOptions where
  def = ChartOptions def def def def def def def def []
