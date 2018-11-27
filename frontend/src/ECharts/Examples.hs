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
import Data.Time.Calendar
import System.Random

import JSDOM.Types (JSVal, toJSVal, JSM, MonadJSM, liftJSM)
import Reflex.Dom.Core

seriesExamples
  :: ( DomBuilder t m
     , PerformEvent t m
     , PostBuild t m
     , MonadHold t m
     , MonadJSM (Performable m)
     , GhcjsDomSpace ~ DomBuilderSpace m
     , RandomGen g
     )
  => g
  -> m ()
seriesExamples rGen = elAttr "div" ("style" =: "display: flex; flex-wrap: wrap") $
  mapM_ renderChartOptions $ reverse
    [ basicLineChart
    , basicAreaChart
    , smoothedLineChart
    , stackedAreaChart
    , rainfall
    , largeScaleAreaChart rGen
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
  e <- fst <$> elAttr' "div" ("style" =: "width:600px; height:400px; padding: 50px;") blank
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
  , _chartOptions_legend = Just $ def
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
  , _chartOptions_legend = Just $ def
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
        & series_areaStyle ?~ def
        & series_lineStyle ?~ def { _lineStyle_width = Just 1 }
        & series_markArea ?~ def
        { _markArea_silent = Just True
        , _markArea_data = Just $ Aeson.Array $ V.singleton $ Aeson.Array $ V.fromList
            [ Aeson.Object $ HashMap.singleton "xAxis"
              (Aeson.String $ dateF 9 12 7)
            , Aeson.Object $ HashMap.singleton "xAxis"
              (Aeson.String $ dateF 9 22 7)
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
            [ Aeson.Object $ HashMap.singleton "xAxis"
              (Aeson.String $ dateF 9 10 7)
            , Aeson.Object $ HashMap.singleton "xAxis"
              (Aeson.String $ dateF 9 20 7)
            ]
        }
    ]
  }
  where
    xSeriesName = "Water flow"
    ySeriesName = "Rainfall"
    xAxisData = [dateF 6 12 t | t <- [2..23]]
      <> [dateF 6 d t | d <- [13..30], t <- [0..23]]
      <> [dateF 7 d t | d <- [1..31], t <- [0..23]]
      <> [dateF 8 d t | d <- [1..31], t <- [0..23]]
      <> [dateF 9 d t | d <- [1..30], t <- [0..23]]
      <> [dateF 10 d t | d <- [1..17], t <- [0..23]]
      <> [dateF 10 18 t | t <- [0..8]]
    dateF m d t = "2009/" <> tshow m <> "/" <> tshow d <> "\n" <> tshow t <> ":00"

tshow :: (Show a) => a -> Text
tshow = T.pack . show

largeScaleAreaChart :: RandomGen g => g -> ChartOptions
largeScaleAreaChart rGen = def
  { _chartOptions_title = def
    {
      _title_text = Just "Large Scale Area Chart"
    , _title_position = Just $ def {
        _position_left = Just $ PosAlign_Align Align_Center
        }
    }
  , _chartOptions_tooltip = def
    { _toolTip_trigger = Just "axis"
    -- TODO
    -- , _toolTip_position = 
    }
  , _chartOptions_toolbox = def
    { _toolBox_features =
      [ emptyDataZoom { _feature_yAxisIndex = Just $ Aeson.String "none" }
      , emptyRestore
      , emptySaveAsImage
      ]
    }
  , _chartOptions_dataZoom =
    [ def
      { _dataZoom_show = Just True
      , _dataZoom_handleSize = Just (SN_String "80%")
      , _dataZoom_handleIcon = Just "M10.7,11.9v-1.3H9.3v1.3c-4.9,0.3-8.8,4.4-8.8,9.4c0,5,3.9,9.1,8.8,9.4v1.3h1.3v-1.3c4.9-0.3,8.8-4.4,8.8-9.4C19.5,16.3,15.6,12.2,10.7,11.9z M13.3,24.4H6.7V23h6.6V24.4z M13.3,19.6H6.7v-1.4h6.6V19.6z"
      , _dataZoom_handleStyle = Just $ def
        { _itemStyle_color = Just "#fff"
        , _itemStyle_shadow = Just $ def
          { _shadow_blur = Just 3
          , _shadow_color = Just "rgba(0, 0, 0, 0.6)"
          , _shadow_offsetX = Just 2
          , _shadow_offsetY = Just 2
          }
        }
      , _dataZoom_start = Just $ Aeson.Number 0
      , _dataZoom_end = Just $ Aeson.Number 10
      }
    , def
      { _dataZoom_type = Just "inside"
      , _dataZoom_start = Just $ Aeson.Number 0
      , _dataZoom_end = Just $ Aeson.Number 10
      }
    ]
  , _chartOptions_xAxis = def { _axis_type = Just AxisType_Category
                              , _axis_data = Just $ zip xAxisData $ repeat Nothing
                              , _axis_boundaryGap = Just $ Left False} :[]
  , _chartOptions_yAxis =
    [ def { _axis_type = Just AxisType_Value
          , _axis_boundaryGap = Just $ Right (SizeValue_Numeric 0, SizeValue_Percent 100)
          }
    ]
  , _chartOptions_series =
    [ Some.This $ SeriesT_Line $ def
        & series_data ?~ (map (DataNumber . fromFloatDigits) randomData)
        & series_name ?~ xSeriesName
        & series_smooth ?~ Left True
        & series_itemStyle ?~ def { _itemStyle_color = Just "rgb(255, 70, 131)" }
        -- TODO uses new
        & series_areaStyle ?~ def
    ]
  }
  where
    xSeriesName = "Random Data"
    xAxisData = take dataSize $ map (\d -> tshow $ addDays d startDate) [0..]
    startDate = fromGregorian 1968 9 3
    dataSize = 20000
    rs :: [Double]
    rs = randomRs (-0.5, 0.5) rGen
    randomData = take dataSize $ scanl (\d r -> r * 20 + d) 50 rs
