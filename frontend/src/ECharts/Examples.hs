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
import ECharts.ExamplesData (rainfallData, waterFlowData)

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Scientific
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time
import Control.Lens
import qualified Data.Some as Some
import qualified Data.Aeson as Aeson
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Vector as V

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
  , rainfall
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
  { _chartOptions_xAxis = [def { _axis_type = Just AxisType_Category
                              , _axis_data = Just $ zip xAxisData $ repeat Nothing}
                          ]
  , _chartOptions_yAxis = [def { _axis_type = Just AxisType_Value }]
  , _chartOptions_series = [Some.This $ SeriesT_Line $ def
    & series_data ?~ (map DataNumber yAxisData)]
  }
  where
    xAxisData = ["Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"]
    yAxisData = [820, 932, 901, 934, 1290, 1330, 1320]

basicAreaChart :: ChartOptions
basicAreaChart = def
  { _chartOptions_xAxis = [def { _axis_type = Just AxisType_Category
                              , _axis_data = Just $ zip xAxisData $ repeat Nothing
                              , _axis_boundaryGap = Just $ Left False
                              }
                          ]
  , _chartOptions_yAxis = [def { _axis_type = Just AxisType_Value }]
  , _chartOptions_series = [Some.This $ SeriesT_Line $ def
    & series_data ?~ (map DataNumber yAxisData)
    & series_areaStyle ?~ def ]
  }
  where
    xAxisData = ["Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"]
    yAxisData = [820, 932, 901, 934, 1290, 1330, 1320]

smoothedLineChart :: ChartOptions
smoothedLineChart = def
  { _chartOptions_xAxis = [def { _axis_type = Just AxisType_Category
                              , _axis_data = Just $ zip xAxisData $ repeat Nothing}
                          ]
  , _chartOptions_yAxis = [def { _axis_type = Just AxisType_Value }]
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
  , _chartOptions_tooltip = def
    { _toolTip_trigger = Just "axis"
    , _toolTip_axisPointer = Just $ Aeson.Object $ HashMap.fromList
      [ ("type", Aeson.String "cross")
      , ("label", Aeson.Object $ HashMap.singleton "backgroundColor" "#6a7985")
      ]
    }
  , _chartOptions_toolbox = def
    { _toolBox_features =
      [ emptySaveAsImage { _feature_title = Just "Save as PNG" }
      ]
    }
  , _chartOptions_legend = def
    { _legend_data = Just $ [ ("A", def)
                            , ("B", def)
                            , ("C", def)
                            , ("D", def)
                            , ("E", def)
                            ]}
  , _chartOptions_grid = def
    { _grid_position = Just
      (def { _position_left = Just $ PosAlign_Percent 3
                   , _position_right = Just $ PosAlign_Percent 4
                   , _position_bottom = Just $ PosAlign_Percent 3})
            , _grid_containLabel = Just True
    }
  , _chartOptions_xAxis = def { _axis_type = Just AxisType_Category
                              , _axis_data = Just $ zip xAxisData $ repeat Nothing
                              , _axis_boundaryGap = Just $ Left False} : []
  , _chartOptions_yAxis = def { _axis_type = Just AxisType_Value
                              } : []
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

rainfall :: ChartOptions
rainfall = def
  { _chartOptions_title = def
    {
      _title_text = Just "Rainfall/Water volume"
    , _title_subtext = Just "Flow of water and rainfall"
    , _title_position = Just $ def {
        _position_left = Just $ PosAlign_Align Align_Center
        }
    }
  , _chartOptions_grid = def
    { _grid_position = Just $ def { _position_bottom = Just $ PosAlign_Pixel 80 }
    }
  , _chartOptions_toolbox = def
    { _toolBox_features =
      [ emptyDataZoom { _feature_yAxisIndex = Just $ Aeson.String "none" }
      , emptyRestore
      , emptySaveAsImage
      ]
    }
  , _chartOptions_tooltip = def
    { _toolTip_trigger = Just "axis"
    , _toolTip_axisPointer = Just $ Aeson.Object $ HashMap.fromList
      [ ("type", Aeson.String "cross")
      , ("label", Aeson.Object $ HashMap.singleton "backgroundColor" "#505765")
      ]
    }
  , _chartOptions_legend = def
    { _legend_data = Just $ [ (xSeriesName, def)
                            , (ySeriesName, def)
                            ]
    , _legend_position = Just $ def {_position_left = Just $ PosAlign_Align Align_Left }
    }
  , _chartOptions_dataZoom =
    [ def
      { _dataZoom_show = Just True
      , _dataZoom_realtime = Just True
      , _dataZoom_start = Just $ Aeson.Number 65
      , _dataZoom_end = Just $ Aeson.Number 85
      }
    , def
      { _dataZoom_type = Just "inside"
      , _dataZoom_realtime = Just True
      , _dataZoom_start = Just $ Aeson.Number 65
      , _dataZoom_end = Just $ Aeson.Number 85
      }
    ]
  , _chartOptions_xAxis = def { _axis_type = Just AxisType_Category
                              , _axis_data = Just $ zip xAxisData $ repeat Nothing
                              , _axis_axisLine = Just $ def
                                { _axisLine_onZero = Just False }
                              , _axis_boundaryGap = Just $ Left False} :[]
  , _chartOptions_yAxis =
    [ def { _axis_type = Just AxisType_Value
          , _axis_name = Just "Water Flow (m^3/s)"
          , _axis_max = Just $ Left 500
          }
    , def { _axis_type = Just AxisType_Value
          , _axis_name = Just "Rainfall (mm)"
          , _axis_max = Just $ Left 5
          , _axis_inverse = Just $ True
          , _axis_nameLocation = Just $ AxisNameLocation_Start
          }
    ]
  , _chartOptions_series =
    [ Some.This $ SeriesT_Line $ def
        & series_data ?~ (map (DataNumber . fromFloatDigits) waterFlowData)
        & series_name ?~ xSeriesName
        & series_animation ?~ False
        & series_yAxisIndex ?~ 0
        & series_areaStyle ?~ def
        & series_lineStyle ?~ def { _lineStyle_width = Just 1 }
        & series_markArea ?~ def
        { _markArea_silent = Just True
        , _markArea_data = Just $ Aeson.Array $ V.singleton $ Aeson.Array $ V.fromList
            [ Aeson.Object $ HashMap.singleton "xAxis" "2009/9/12\n7:00"
            , Aeson.Object $ HashMap.singleton "xAxis" "2009/9/22\n7:00"
            ]
        }
    , Some.This $ SeriesT_Line $ def
        & series_data ?~ (map (DataNumber . fromFloatDigits) rainfallData)
        & series_name ?~ ySeriesName
        & series_yAxisIndex ?~ 1
        & series_animation ?~ False
        & series_areaStyle ?~ def
        & series_lineStyle ?~ def { _lineStyle_width = Just 1 }
        & series_markArea ?~ def
        { _markArea_silent = Just True
        , _markArea_data = Just $ Aeson.Array $ V.singleton $ Aeson.Array $ V.fromList
            [ Aeson.Object $ HashMap.singleton "xAxis" "2009/9/10\n7:00"
            , Aeson.Object $ HashMap.singleton "xAxis" "2009/9/20\n7:00"
            ]
        }
    ]
  }
  where
    xSeriesName = "Water flow"
    ySeriesName = "Rainfall"
    xAxisData = [f 6 12 t | t <- [2..23]]
      <> [f 6 d t | d <- [13..30], t <- [0..23]]
      <> [f 7 d t | d <- [1..31], t <- [0..23]]
      <> [f 8 d t | d <- [1..31], t <- [0..23]]
      <> [f 9 d t | d <- [1..30], t <- [0..23]]
      <> [f 10 d t | d <- [1..17], t <- [0..23]]
      <> [f 10 18 t | t <- [0..8]]
    f m d t = "2009/" <> tshow m <> "/" <> tshow d <> "\n" <> tshow t <> ":00"

tshow = T.pack . show
