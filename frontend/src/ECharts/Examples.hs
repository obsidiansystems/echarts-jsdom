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
seriesExamples = mapM_ renderChartOptions
  [ basicLineChart
  , basicAreaChart
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
    & series_areaStyle ?~ def {_areaStyle_origin = Just "auto"}]
  }
  where
    xAxisData = ["Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"]
    yAxisData = [820, 932, 901, 934, 1290, 1330, 1320]
