{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
module ECharts.Examples where

import ECharts
import ECharts.Internal
import ECharts.Types
import ECharts.Series
import ECharts.Data
import ECharts.ChartOptions

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Scientific
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time
import Control.Lens
import qualified Data.Some as Some

import JSDOM.Types (JSVal, toJSVal, JSM, MonadJSM, liftJSM)
import Reflex.Dom.Core

seriesExamples
  :: ( DomBuilder t m
     , PerformEvent t m
     , PostBuild t m
     , MonadHold t m
     , MonadJSM (Performable m)
     , GhcjsDomSpace ~ DomBuilderSpace m
     )
  => m ()
seriesExamples = mapM_ renderChartOptions $ reverse
  [ basicLineChart
  , basicAreaChart
  , smoothedLineChart
  , stackedAreaChart
  ]

renderChartOptions
  :: ( DomBuilder t m
     , PerformEvent t m
     , PostBuild t m
     , MonadHold t m
     , MonadJSM (Performable m)
     , GhcjsDomSpace ~ DomBuilderSpace m
     )
  => ChartOptions
  -> m ()
renderChartOptions opts = do
  e <- fst <$> elAttr' "div" ("style" =: "width:600px; height:400px;") blank
  p <- getPostBuild
  chart <- performEvent $ ffor p $ \_ -> liftJSM $ ECharts.init $ _element_raw e
  performEvent_ $ ffor chart $ \c -> liftJSM $ setOption c opts

basicLineChart :: ChartOptions
basicLineChart = def
  { _chartOptions_xAxis = def { _axis_type = Just AxisType_Category
                              , _axis_data = Just $ zip xAxisData $ repeat Nothing}
  , _chartOptions_yAxis = def { _axis_type = Just AxisType_Value
                              }
  , _chartOptions_series = [Some.This $ SeriesT_Line $ def
    & series_data ?~ (map DataNumber yAxisData)]
  }
  where
    xAxisData = ["Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"]
    yAxisData = [820, 932, 901, 934, 1290, 1330, 1320]

basicAreaChart :: ChartOptions
basicAreaChart = def
  { _chartOptions_xAxis = def { _axis_type = Just AxisType_Category
                              , _axis_data = Just $ zip xAxisData $ repeat Nothing
                              , _axis_boundaryGap = Just $ Left False}
  , _chartOptions_yAxis = def { _axis_type = Just AxisType_Value
                              }
  , _chartOptions_series = [Some.This $ SeriesT_Line $ def
    & series_data ?~ (map DataNumber yAxisData)
    & series_areaStyle ?~ def ]
  }
  where
    xAxisData = ["Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"]
    yAxisData = [820, 932, 901, 934, 1290, 1330, 1320]

smoothedLineChart :: ChartOptions
smoothedLineChart = def
  { _chartOptions_xAxis = def { _axis_type = Just AxisType_Category
                              , _axis_data = Just $ zip xAxisData $ repeat Nothing}
  , _chartOptions_yAxis = def { _axis_type = Just AxisType_Value
                              }
  , _chartOptions_series = [Some.This $ SeriesT_Line $ def
    & series_data ?~ (map DataNumber yAxisData)
    & series_smooth ?~ Left True]
  }
  where
    xAxisData = ["Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"]
    yAxisData = [820, 932, 901, 934, 1290, 1330, 1320]

stackedAreaChart :: ChartOptions
stackedAreaChart = def
  { _chartOptions_title = def { _title_text = Just title }
  , _chartOptions_grid = def { _grid_position = Just
                               (def { _position_left = Just $ PosAlign_Percent 3
                                    , _position_right = Just $ PosAlign_Percent 4
                                    , _position_bottom = Just $ PosAlign_Percent 3})
                             , _grid_containLabel = Just True
                             }
  , _chartOptions_xAxis = def { _axis_type = Just AxisType_Category
                              , _axis_data = Just $ zip xAxisData $ repeat Nothing
                              , _axis_boundaryGap = Just $ Left False}
  , _chartOptions_yAxis = def { _axis_type = Just AxisType_Value
                              }
  , _chartOptions_series = [l1, l2, l3, l4, l5]
  }
  where
    title = "Stacked Area Chart"
    xAxisData = ["Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"]
    stackLabel = "stackLabel"
    l1 = Some.This $ SeriesT_Line $ def
      & series_stack ?~ stackLabel
      & series_name ?~ "A"
      & series_areaStyle ?~ def
      & series_data ?~
      (map DataNumber [120, 132, 101, 134, 90, 230, 210])
    l2 = Some.This $ SeriesT_Line $ def
      & series_stack ?~ stackLabel
      & series_name ?~ "B"
      & series_areaStyle ?~ def
      & series_data ?~
      (map DataNumber [220, 182, 191, 234, 290, 330, 310])
    l3 = Some.This $ SeriesT_Line $ def
      & series_stack ?~ stackLabel
      & series_name ?~ "C"
      & series_areaStyle ?~ def
      & series_data ?~
      (map DataNumber [150, 232, 201, 154, 190, 330, 410])
    l4 = Some.This $ SeriesT_Line $ def
      & series_stack ?~ stackLabel
      & series_name ?~ "D"
      & series_areaStyle ?~ def
      & series_data ?~
      (map DataNumber [320, 332, 301, 334, 390, 330, 320])
    l5 = Some.This $ SeriesT_Line $ def
      & series_stack ?~ stackLabel
      & series_name ?~ "E"
      & series_areaStyle ?~ def
      & series_label ?~ def { _label_show = Just True, _label_position = Just "top"}
      & series_data ?~
      (map DataNumber [820, 932, 901, 934, 1290, 1330, 1320])
