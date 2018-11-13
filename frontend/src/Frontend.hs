{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
module Frontend where

import Data.Aeson (ToJSON, genericToEncoding, defaultOptions, Options(..))
import qualified Data.Aeson as Aeson
import GHC.Generics (Generic)
import qualified Data.Text as T
import Data.Text (Text)
import Obelisk.Frontend
import Obelisk.Route
import Reflex.Dom.Core

import Common.Api
import Common.Route
import Obelisk.Generated.Static

import JSDOM.Generated.Element hiding (Element)
import qualified JSDOM.Generated.Element as JSDOM
import JSDOM.Types hiding (Text)
import Language.Javascript.JSaddle.Evaluate
import Language.Javascript.JSaddle.Object
import Data.Map (Map)
import qualified Data.Map as Map

data ECharts = ECharts { unECharts :: JSVal }

init :: JSDOM.Element -> JSM ECharts
init e = do
  f <- eval $ T.pack "(function(e) { return echarts['init'](e) })"
  arg <- toJSVal e
  ECharts <$> call f f [arg]

data Target = Target_Blank
            | Target_Self

instance ToJSON Target where
  toJSON Target_Blank = Aeson.String "blank"
  toJSON Target_Self = Aeson.String "self"

data FontStyle = FontStyle_Normal
               | FontStyle_Italic
               | FontStyle_Oblique

instance ToJSON FontStyle where
  toJSON FontStyle_Normal = Aeson.String "normal"
  toJSON FontStyle_Italic = Aeson.String "italic"
  toJSON FontStyle_Oblique = Aeson.String "oblique"

data FontWeight = FontWeight_Normal
                | FontWeight_Bold
                | FontWeight_Bolder
                | FontWeight_Lighter
                | FontWeight_Numeric Int

instance ToJSON FontWeight where
  toJSON FontWeight_Normal = Aeson.String "normal"
  toJSON FontWeight_Bold = Aeson.String "bold"
  toJSON FontWeight_Bolder = Aeson.String "bolder"
  toJSON FontWeight_Lighter = Aeson.String "lighter"
  toJSON (FontWeight_Numeric n) = Aeson.Number $ realToFrac n

data FontFamily = FontFamily_Serif
                | FontFamily_SansSerif
                | FontFamily_Monospace
                | FontFamily_Other Text

instance ToJSON FontFamily where
  toJSON FontFamily_Serif = Aeson.String "serif"
  toJSON FontFamily_SansSerif = Aeson.String "sans-serif"
  toJSON FontFamily_Monospace = Aeson.String "monospace"
  toJSON (FontFamily_Other t) = Aeson.String t

data Font = Font
  { _font_style :: Maybe FontStyle
  , _font_weight :: Maybe FontWeight
  , _font_family :: Maybe FontFamily
  , _font_size :: Maybe Int
  }

data Align = Align_Auto
           | Align_Left
           | Align_Center
           | Align_Right

data VerticalAlign = VerticalAlign_Auto
                   | VerticalAlign_Top
                   | VerticalAlign_Middle
                   | VerticalAlign_Bottom

data SizeValue = SizeValue_Auto
               | SizeValue_Percent Int
               | SizeValue_Numeric Int

data TextStyle = TextStyle
  { _textStyle_color :: Maybe Text
  , _textStyle_font :: Maybe Font
  , _textStyle_align :: Maybe Align
  , _textStyle_verticalAlign :: Maybe VerticalAlign
  , _textStyle_lineHeight :: Maybe Int
  , _textStyle_width :: Maybe SizeValue
  , _textStyle_textBorder :: Maybe Border
  , _textStyle_textShadow :: Maybe Shadow
  , _textStyle_rich :: Maybe Text
  }

data Border = Border
  { _border_color :: Maybe Text
  , _border_width :: Maybe Int
  , _border_radius :: Maybe (Int, Int, Int, Int)
  }

data Shadow = Shadow
  { _shadow_color :: Maybe Text
  , _shadow_blur :: Maybe Int
  , _shadow_offsetX :: Maybe Int
  , _shadow_offsetY :: Maybe Int
  }

data PosAlign = PosAlign_Auto
              | PosAlign_Pixel Int
              | PosAlign_Percent Int
              | PosAlign_Align Align

data Position = Position
  { _position_zlevel :: Maybe Int
  , _position_z :: Maybe Int
  , _position_left :: Maybe PosAlign
  , _position_top :: Maybe PosAlign
  , _position_right :: Maybe PosAlign
  , _position_bottom :: Maybe PosAlign
  }

data Size = Size
  { _size_width :: Maybe SizeValue
  , _size_height :: Maybe SizeValue
  }

data Orientation = Orientation_Horizontal
                 | Orientation_Vertical
                 | Orientation_Auto

data Title = Title
  { _title_show :: Maybe Bool
  , _title_text :: Maybe Text
  , _title_link :: Maybe Text
  , _title_target :: Maybe Target
  , _title_textStyle :: Maybe TextStyle
  , _title_subtext :: Maybe Text
  , _title_sublink :: Maybe Text
  , _title_subtarget :: Maybe Target
  , _title_subtextStyle :: Maybe TextStyle
  , _title_triggerEvent :: Maybe Bool
  , _title_padding :: Maybe (Int, Int, Int, Int)
  , _title_itemGap :: Maybe Int
  , _title_backgroundColor :: Maybe Text
  , _title_border :: Maybe Border
  , _title_shadow :: Maybe Shadow
  , _title_position :: Maybe Position
  }

data LegendType = LegendType_Plain
                | LegendType_Scroll

data Icon = Icon_Circle
          | Icon_Rect
          | Icon_RoundRect
          | Icon_Triangle
          | Icon_Diamond
          | Icon_Pin
          | Icon_Arrow
          | Icon_None
          | Icon_Image Text -- URL
          | Icon_DataURI Text
          | Icon_SVGPath Text

data LegendData = LegendData
  { _legendData_name :: Maybe Text
  , _legendData_icon :: Maybe Icon
  , _legendData_textStyle :: Maybe TextStyle
  }

data PageButtonPosition = PageButtonPosition_Start
                        | PageButtonPosition_End

data Legend = Legend
  { _legend_type :: Maybe LegendType
  , _legend_show :: Maybe Bool
  , _legend_position :: Maybe Position
  , _legend_size :: Maybe Size
  , _legend_orient :: Maybe Orientation
  , _legend_align :: Maybe Align
  , _legend_padding :: Maybe (Int, Int, Int, Int)
  , _legend_itemGap :: Maybe Int
  , _legend_itemWidth :: Maybe Int
  , _legend_itemHeight :: Maybe Int
  , _legend_symbolKeepAspect :: Maybe Bool
  , _legend_formatter :: Maybe Text
  , _legend_selectedMode :: Maybe Bool
  , _legend_inactiveColor :: Maybe Text
  , _legend_selected :: Maybe (Map Text Bool)
  , _legend_textStyle :: Maybe TextStyle
  -- , _legend_tooltip :: TODO
  , _legend_data :: Maybe (Map Text LegendData)
  , _legend_backgroundColor :: Text
  , _legend_border :: Maybe Border
  , _legend_shadow :: Maybe Shadow
  , _legend_scrollDataIndex :: Maybe Int
  , _legend_pageButtonItemGap :: Maybe Int
  , _legend_pageButtonGap :: Maybe Int
  , _legend_pageButtonPosition :: Maybe PageButtonPosition
  , _legend_pageFormatter :: Maybe Text
  -- , _legend_pageIcons :: Maybe  TODO
  , _legend_pageTextStyle :: Maybe TextStyle
  , _legend_animation :: Maybe Bool
  , _legend_animationDurationUpdate :: Maybe Int
  }

data Grid = Grid
  { _grid_show :: Maybe Bool
  , _grid_position :: Maybe Position
  , _grid_size :: Maybe Size
  , _grid_containLabel :: Maybe Bool
  , _grid_backgroundColor :: Maybe Text
  , _grid_border :: Maybe Border
  , _grid_shadow :: Maybe Shadow
  -- , _grid_tooltip :: Maybe TODO
  }

data AxisPosition = AxisPosition_Top
                   | AxisPosition_Bottom

data AxisType = AxisType_Value
               | AxisType_Category
               | AxisType_Time
               | AxisType_Log

data AxisNameLocation = AxisNameLocation_Start
                       | AxisNameLocation_Center
                       | AxisNameLocation_End

data Axis = Axis
  { _axis_show :: Maybe Bool
  , _axis_gridIndex :: Maybe Int
  , _axis_position :: Maybe AxisPosition
  , _axis_offset :: Maybe Int
  , _axis_type :: Maybe AxisType
  , _axis_name :: Maybe Text
  , _axis_nameLocation :: Maybe AxisNameLocation
  , _axis_nameTextStyle :: Maybe TextStyle
  , _axis_nameGap :: Maybe Int
  , _axis_nameRotate :: Maybe Int
  , _axis_inverse :: Maybe Bool
  , _axis_boundaryGap :: Maybe (Either Bool (SizeValue, SizeValue))
  -- , _axis_min :: Maybe TODO
  -- , _axis_max :: Maybe TODO
  , _axis_scale :: Maybe Bool
  , _axis_minInterval :: Maybe Int
  , _axis_interval :: Maybe Int
  , _axis_logBase :: Maybe Int
  , _axis_silent :: Maybe Bool
  , _axis_triggerEvent :: Maybe Bool
  , _axis_axisLine :: Maybe AxisLine
  , _axis_axisTick :: Maybe AxisTick
  , _axis_axisLabel :: Maybe AxisLabel
  -- , _axis_splitLine :: Maybe SplitLine TODO
  -- , _axis_splitArea :: Maybe SplitArea TODO
  , _axis_data :: Maybe (Map Text (Maybe TextStyle))
  -- , _axis_pointer :: Maybe AxisPointer TODO
  , _axis_zlevel :: Maybe Int
  , _axis_z :: Maybe Int
  }

data AxisLine = AxisLine
  { _axisLine_show :: Maybe Bool
  , _axisLine_onZero :: Maybe Bool
  , _axisLine_onZeroAxisIndex :: Maybe Int
  , _axisLine_symbol :: Maybe (Text, Text)
  , _axisLine_symbolSize :: Maybe (Int, Int)
  , _axisLine_symbolOffset :: Maybe (Int, Int)
  , _axisLine_lineStyle :: Maybe LineStyle
  }

data LineStyleType = LineStyleType_Solid
                   | LineStyleType_Dashed
                   | LineStyleType_Dotted

instance ToJSON LineStyleType where
  toJSON LineStyleType_Solid = Aeson.String "solid"
  toJSON LineStyleType_Dashed = Aeson.String "dashed"
  toJSON LineStyleType_Dotted = Aeson.String "dotted"

data LineStyle = LineStyle
  { _lineStyle_color :: Maybe Text
  , _lineStyle_width :: Maybe Int
  , _lineStyle_type :: Maybe LineStyleType
  , _lineStyle_opacity :: Maybe Double
  , _lineStyle_shadow :: Maybe Shadow
  }

data AxisTick = AxisTick
  { _axisTick_show :: Maybe Bool
  , _axisTick_alignWithLabel :: Maybe Bool
  -- , _axisTick_interval :: Maybe TODO
  , _axisTick_inside :: Maybe Bool
  , _axisTick_length :: Maybe Int
  , _axisTick_lineStyle :: Maybe LineStyle
  }

data AxisLabel = AxisLabel
  { _axisLabel_show :: Maybe Bool
  -- , _axisLabel_interval  TODO
  , _axisLabel_inside :: Maybe Bool
  , _axisLabel_rotate :: Maybe Int
  , _axisLabel_margin :: Maybe Int
  -- , _axisLabel_formatter :: Maybe TODO
  , _axisLabel_showMinLabel :: Maybe Bool
  , _axisLabel_showMaxLabel :: Maybe Bool
  -- , _axisLabel_color :: Maybe TODO
  , _axisLabel_font :: Maybe Font
  , _axisLabel_align :: Maybe Align
  , _axisLabel_verticalAlign :: Maybe VerticalAlign
  , _axisLabel_lineHeight :: Maybe Int
  , _axisLabel_backgroundColor :: Maybe Text
  , _axisLabel_border :: Maybe Border
  , _axisLabel_padding :: Maybe Int
  , _axisLabel_shadow :: Maybe Shadow
  , _axisLabel_size :: Maybe Size
  , _axisLabel_textBorder :: Maybe Border
  , _axisLabel_textShadow :: Maybe Shadow
  -- , _axisLabel_rich :: Maybe TODO
  }

data Series =
    Series_Line
      { _seriesLine_name :: Maybe Text
      , _seriesLine_data :: Maybe [Int]
      , _seriesLine_smooth :: Maybe Bool
      }

data ChartOptions = ChartOptions
  { _chartOptions_title :: Title
  , _chartOptions_legend :: Legend
  , _chartOptions_grid :: Grid
  , _chartOptions_xAxis :: Axis
  , _chartOptions_yAxis :: Axis
  , _chartOptions_series :: Series
  }

data EChartTitle = EChartTitle
  { _eChartTitle_show :: Maybe Bool
  , _eChartTitle_text :: Maybe Text
  , _eChartTitle_link :: Maybe Text
  , _eChartTitle_target :: Maybe Target
  , _eChartTitle_textStyle :: Maybe EChartTextStyle
  , _eChartTitle_subtext :: Maybe Text
  , _eChartTitle_sublink :: Maybe Text
  , _eChartTitle_subtarget :: Maybe Text
  , _eChartTitle_subtextStyle :: Maybe EChartTextStyle
  , _eChartTitle_triggerEvent :: Maybe Bool
  , _eChartTitle_padding :: Maybe Int
  , _eChartTitle_itemGap :: Maybe Int
  , _eChartTitle_zlevel :: Maybe Int
  , _eChartTitle_z :: Maybe Int
  , _eChartTitle_left :: Maybe SN
  , _eChartTitle_right :: Maybe SN
  , _eChartTitle_top :: Maybe SN
  , _eChartTitle_bottom :: Maybe SN
  , _eChartTitle_backgroundColor :: Maybe Text
  , _eChartTitle_borderColor :: Maybe Text
  , _eChartTitle_borderWidth :: Maybe Int
  , _eChartTitle_borderRadius :: Maybe [Int]
  , _eChartTitle_shadowBlur :: Maybe Int
  , _eChartTitle_shadowColor :: Maybe Text
  , _eChartTitle_shadowOffsetX :: Maybe Int
  , _eChartTitle_shadowOffsetY :: Maybe Int
  }
  deriving (Generic)

instance ToJSON EChartTitle where
  toEncoding = genericToEncoding $ defaultOptions
    { fieldLabelModifier = drop $ T.length "_eChartTitle_"
    , omitNothingFields = True
    }

data EChartTextStyle = EChartTextStyle
  { _eChartTextStyle_color :: Maybe Text
  , _eChartTextStyle_fontStyle :: Maybe Text
  , _eChartTextStyle_fontWeight :: Maybe Text
  , _eChartTextStyle_fontFamily :: Maybe Text
  , _eChartTextStyle_fontSize :: Maybe Text
  , _eChartTextStyle_align :: Maybe Text
  , _eChartTextStyle_verticalAlign :: Maybe Text
  , _eChartTextStyle_lineHeight :: Maybe Int
  , _eChartTextStyle_width :: Maybe SN
  , _eChartTextStyle_height :: Maybe SN
  , _eChartTextStyle_textBorderColor :: Maybe Text
  , _eChartTextStyle_textBorderWidth :: Maybe Int
  , _eChartTextStyle_textShadowColor :: Maybe Text
  , _eChartTextStyle_textShadowBlur :: Maybe Int
  , _eChartTextStyle_textShadowOffsetX :: Maybe Int
  , _eChartTextStyle_textShadowOffsetY :: Maybe Int
  }
  deriving (Generic)

instance ToJSON EChartTextStyle where
  toEncoding = genericToEncoding $ defaultOptions
    { fieldLabelModifier = drop $ T.length "_eChartTextStyle_"
    , omitNothingFields = True
    }

data SN = SN_String Text
        | SN_Number Double
        deriving (Generic)

instance ToJSON SN where
  toJSON (SN_String a) = Aeson.String a
  toJSON (SN_Number a) = Aeson.Number $ realToFrac a

data EChartLegend = EChartLegend
  { _eChartLegend_type :: Maybe Text
  , _eChartLegend_show :: Maybe Bool
  , _eChartLegend_zlevel :: Maybe Int
  , _eChartLegend_z :: Maybe Int
  , _eChartLegend_left :: Maybe SN
  , _eChartLegend_top :: Maybe SN
  , _eChartLegend_right :: Maybe SN
  , _eChartLegend_bottom :: Maybe SN
  , _eChartLegend_width :: Maybe SN
  , _eChartLegend_height :: Maybe SN
  , _eChartLegend_orient :: Maybe Text
  , _eChartLegend_align :: Maybe Text
  , _eChartLegend_padding :: Maybe Int
  , _eChartLegend_itemGap :: Maybe Int
  , _eChartLegend_itemWidth :: Maybe Int
  , _eChartLegend_itemHeight :: Maybe Int
  , _eChartLegend_symbolKeepAspect :: Maybe Bool
  , _eChartLegend_formatter :: Maybe Text
  , _eChartLegend_selectedMode :: Maybe Bool
  , _eChartLegend_inactiveColor :: Maybe Text
  , _eChartLegend_selected :: Maybe Aeson.Value
  , _eChartLegend_textStyle :: Maybe EChartTextStyle
  , _eChartLegend_data :: Maybe Aeson.Value
  , _eChartLegend_backgroundColor :: Text
  , _eChartLegend_borderColor :: Maybe Text
  , _eChartLegend_borderWidth :: Maybe Int
  , _eChartLegend_borderRadius :: Maybe (Int, Int, Int, Int)
  , _eChartLegend_shadowColor :: Maybe Text
  , _eChartLegend_shadowBlur :: Maybe Int
  , _eChartLegend_shadowOffsetX :: Maybe Int
  , _eChartLegend_shadowOffsetY :: Maybe Int
  , _eChartLegend_scrollDataIndex :: Maybe Int
  , _eChartLegend_pageButtonItemGap :: Maybe Int
  , _eChartLegend_pageButtonGap :: Maybe Int
  , _eChartLegend_pageButtonPosition :: Maybe Text
  , _eChartLegend_pageFormatter :: Maybe Text
  , _eChartLegend_pageTextStyle :: Maybe EChartTextStyle
  , _eChartLegend_animation :: Maybe Bool
  , _eChartLegend_animationDurationUpdate :: Maybe Int
  }
  deriving (Generic)

instance ToJSON EChartLegend where
  toEncoding = genericToEncoding $ defaultOptions
    { fieldLabelModifier = drop $ T.length "_eChartLegend_"
    , omitNothingFields = True
    }

data EChartAxis = EChartAxis
  { _eChartAxis_show :: Maybe Bool
  , _eChartAxis_gridIndex :: Maybe Int
  , _eChartAxis_position :: Maybe Text
  , _eChartAxis_offset :: Maybe Int
  , _eChartAxis_type :: Maybe Text
  , _eChartAxis_name :: Maybe Text
  , _eChartAxis_nameLocation :: Maybe Text
  , _eChartAxis_nameTextStyle :: Maybe EChartTextStyle
  , _eChartAxis_nameGap :: Maybe Int
  , _eChartAxis_nameRotate :: Maybe Int
  , _eChartAxis_inverse :: Maybe Bool
  , _eChartAxis_boundaryGap :: Maybe Aeson.Value
  , _eChartAxis_scale :: Maybe Bool
  , _eChartAxis_minInterval :: Maybe Int
  , _eChartAxis_interval :: Maybe Int
  , _eChartAxis_logBase :: Maybe Int
  , _eChartAxis_silent :: Maybe Bool
  , _eChartAxis_triggerEvent :: Maybe Bool
  , _eChartAxis_axisLine :: Maybe EChartAxisLine
  , _eChartAxis_axisTick :: Maybe EChartAxisTick
  , _eChartAxis_axisLabel :: Maybe EChartAxisLabel
  , _eChartAxis_data :: Maybe Aeson.Value
  , _eChartAxis_zlevel :: Maybe Int
  , _eChartAxis_z :: Maybe Int
  }
  deriving (Generic)

instance ToJSON EChartAxis where
  toEncoding = genericToEncoding $ defaultOptions
    { fieldLabelModifier = drop $ T.length "_eChartAxis_"
    , omitNothingFields = True
    }

data EChartAxisLine = EChartAxisLine
  { _eChartAxisLine_show :: Maybe Bool
  , _eChartAxisLine_onZero :: Maybe Bool
  , _eChartAxisLine_onZeroAxisIndex :: Maybe Int
  , _eChartAxisLine_symbol :: Maybe (Text, Text)
  , _eChartAxisLine_symbolSize :: Maybe (Int, Int)
  , _eChartAxisLine_symbolOffset :: Maybe (Int, Int)
  , _eChartAxisLine_lineStyle :: Maybe EChartLineStyle
  }
  deriving (Generic)

instance ToJSON EChartAxisLine where
  toEncoding = genericToEncoding $ defaultOptions
    { fieldLabelModifier = drop $ T.length "_eChartAxisLine_"
    , omitNothingFields = True
    }

data EChartLineStyle = EChartLineStyle
  { _eChartLineStyle_color :: Maybe Text
  , _eChartLineStyle_width :: Maybe Int
  , _eChartLineStyle_type :: Maybe LineStyleType
  , _eChartLineStyle_opacity :: Maybe Double
  , _eChartShadow_color :: Maybe Text
  , _eChartShadow_blur :: Maybe Int
  , _eChartShadow_offsetX :: Maybe Int
  , _eChartShadow_offsetY :: Maybe Int
  }
  deriving (Generic)

instance ToJSON EChartLineStyle where
  toEncoding = genericToEncoding $ defaultOptions
    { fieldLabelModifier = drop $ T.length "_eChartLineStyle_"
    , omitNothingFields = True
    }

data EChartAxisTick = EChartAxisTick
  { _eChartAxisTick_show :: Maybe Bool
  , _eChartAxisTick_alignWithLabel :: Maybe Bool
  , _eChartAxisTick_inside :: Maybe Bool
  , _eChartAxisTick_length :: Maybe Int
  , _eChartAxisTick_lineStyle :: Maybe EChartLineStyle
  }
  deriving (Generic)

instance ToJSON EChartAxisTick where
  toEncoding = genericToEncoding $ defaultOptions
    { fieldLabelModifier = drop $ T.length "_eChartAxisTick_"
    , omitNothingFields = True
    }

data EChartAxisLabel = EChartAxisLabel
  { _eChartAxisLabel_show :: Maybe Bool
  , _eChartAxisLabel_inside :: Maybe Bool
  , _eChartAxisLabel_rotate :: Maybe Int
  , _eChartAxisLabel_margin :: Maybe Int
  , _eChartAxisLabel_showMinLabel :: Maybe Bool
  , _eChartAxisLabel_showMaxLabel :: Maybe Bool
  , _eChartAxisLabel_fontStyle :: Maybe FontStyle
  , _eChartAxisLabel_fontWeight :: Maybe FontWeight
  , _eChartAxisLabel_fontFamily :: Maybe FontFamily
  , _eChartAxisLabel_fontSize :: Maybe Int
  , _eChartAxisLabel_align :: Maybe Text
  , _eChartAxisLabel_verticalAlign :: Maybe Text
  , _eChartAxisLabel_lineHeight :: Maybe Int
  , _eChartAxisLabel_backgroundColor :: Maybe Text
  , _eChartAxisLabel_borderColor :: Maybe Text
  , _eChartAxisLabel_borderWidth :: Maybe Int
  , _eChartAxisLabel_borderRadius :: Maybe (Int, Int, Int, Int)
  , _eChartAxisLabel_padding :: Maybe Int
  , _eChartAxisLabel_shadowColor :: Maybe Text
  , _eChartAxisLabel_shadowBlur :: Maybe Int
  , _eChartAxisLabel_shadowOffsetX :: Maybe Int
  , _eChartAxisLabel_shadowOffsetY :: Maybe Int
  , _eChartAxisLabel_width :: Maybe SN
  , _eChartAxisLabel_height :: Maybe SN
  , _eChartAxisLabel_textBorderColor :: Maybe Text
  , _eChartAxisLabel_textBorderWidth :: Maybe Int
  , _eChartAxisLabel_textBorderRadius :: Maybe (Int, Int, Int, Int)
  , _eChartAxisLabel_textShadowColor :: Maybe Text
  , _eChartAxisLabel_textShadowBlur :: Maybe Int
  , _eChartAxisLabel_textShadowOffsetX :: Maybe Int
  , _eChartAxisLabel_textShadowOffsetY :: Maybe Int
  }
  deriving (Generic)

instance ToJSON EChartAxisLabel where
  toEncoding = genericToEncoding $ defaultOptions
    { fieldLabelModifier = drop $ T.length "_eChartAxisLabel_"
    , omitNothingFields = True
    }

data EChartSeries = EChartSeries
  { _eChartSeries_type :: Maybe Text
  , _eChartSeries_name :: Maybe Text
  , _eChartSeries_data :: Maybe [Int]
  , _eChartSeries_smooth :: Maybe Bool
  }
  deriving (Generic)

instance ToJSON EChartSeries where
  toEncoding = genericToEncoding $ defaultOptions
    { fieldLabelModifier = drop $ T.length "_eChartSeries_"
    , omitNothingFields = True
    }

data EChartConfig = EChartConfig
  { _eChartConfig_title :: EChartTitle
  , _eChartConfig_legend :: EChartLegend
  , _eChartConfig_xAxis :: EChartAxis
  , _eChartConfig_yAxis :: EChartAxis
  , _eChartConfig_series :: [EChartSeries]
  }
  deriving (Generic)

instance ToJSON EChartConfig where
  toEncoding = genericToEncoding $ defaultOptions
    { fieldLabelModifier = drop $ T.length "_eChartConfig_"
    , omitNothingFields = True
    }

toEChartConfig :: ChartOptions -> EChartConfig
toEChartConfig c = EChartConfig
  { _eChartConfig_title = toEChartTitle $ _chartOptions_title c
  -- , _eChartConfig_legend = toEChartLegend $ _chartOptions_legend c
  -- , _eChartConfig_grid = toEChartGrid $ _chartOptions_grid c
  -- , _eChartConfig_xAxis = toEChartAxis $ _chartOptions_xAxis c
  -- , _eChartConfig_yAxis = toEChartAxis $ _chartOptions_yAxis c
  -- , _eChartConfig_series = toEChartSeries $ _chartOptions_series c
  }
  where
    toEChartTextStyle :: TextStyle -> EChartTextStyle
    toEChartTextStyle t = EChartTextStyle
      { _eChartTextStyle_color = _textStyle_color t
      , _eChartTextStyle_fontStyle = _textStyle_fontStyle t
      , _eChartTextStyle_fontWeight = _textStyle_fontWeight t
      , _eChartTextStyle_fontFamily = _textStyle_fontFamily t
      , _eChartTextStyle_fontSize = _textStyle_fontSize t
      , _eChartTextStyle_align = _textStyle_align t
      , _eChartTextStyle_verticalAlign = _textStyle_verticalAlign t
      , _eChartTextStyle_lineHeight = _textStyle_lineHeight t
      , _eChartTextStyle_width = _textStyle_width t
      , _eChartTextStyle_height = _textStyle_height t
      , _eChartTextStyle_textBorderColor = _textStyle_textBorderColor t
      , _eChartTextStyle_textBorderWidth = _textStyle_textBorderWidth t
      , _eChartTextStyle_textShadowColor = _textStyle_textShadowColor t
      , _eChartTextStyle_textShadowBlur = _textStyle_textShadowBlur t
      , _eChartTextStyle_textShadowOffsetX = _textStyle_textShadowOffsetX t
      , _eChartTextStyle_textShadowOffsetY = _textStyle_textShadowOffsetY t
      }
    toEChartTitle :: Title -> EChartTitle
    toEChartTitle t = EChartTitle
      { _eChartTitle_show = _title_show t
      , _eChartTitle_text = _title_text t
      , _eChartTitle_link = _title_link t
      , _eChartTitle_target = _title_target t
      , _eChartTitle_textStyle = toEChartTextStyle $ _title_textStyle t
      , _eChartTitle_subtext = _title_subtext t
      , _eChartTitle_sublink = _title_sublink t
      , _eChartTitle_subtarget = _title_subtarget t
      , _eChartTitle_subtextStyle = toEChartTextStyle $ _title_subtextStyle t
      , _eChartTitle_triggerEvent = _title_triggerEvent t
      , _eChartTitle_padding = _title_padding t
      , _eChartTitle_itemGap = _title_itemGap t
      , _eChartTitle_zlevel = _title_zlevel t
      , _eChartTitle_z = _title_z t
      , _eChartTitle_left = _title_left t
      , _eChartTitle_right = _title_right t
      , _eChartTitle_top = _title_top t
      , _eChartTitle_bottom = _title_bottom t
      , _eChartTitle_backgroundColor = _title_backgroundColor t
      , _eChartTitle_borderColor = _title_borderColor t
      , _eChartTitle_borderWidth = _title_borderWidth t
      , _eChartTitle_borderRadius = _title_borderRadius t
      , _eChartTitle_shadowBlur = _title_shadowBlur t
      , _eChartTitle_shadowColor = _title_shadowColor t
      , _eChartTitle_shadowOffsetX = _title_shadowOffsetX t
      , _eChartTitle_shadowOffsetY = _title_shadowOffsetY t
      }
    -- toEChartLegend l = EChartLegend


example :: JSDOM.Element -> JSM ()
example e = do
  f <- eval $ T.unlines
        [ "(function(e) {"
        , "var myChart = echarts['init'](e);"
        , "var option = { title: { text: 'Example' },"
        , "               tooltip: {},"
        , "               legend: { data: ['Sales'] },"
        , "               xAxis: { data: ["
        , "                 'shirt',"
        , "                 'cardigan',"
        , "                 'chiffon shirt',"
        , "                 'pants',"
        , "                 'heels',"
        , "                 'socks' ]"
        , "               },"
        , "               yAxis: {},"
        , "               series: [{"
        , "                 name: 'Sales',"
        , "                 type: 'bar',"
        , "                 data: [5, 20, 36, 10, 10, 20]"
        , "               }]"
        , "             };"
        , "myChart['setOption'](option);"
        , "})"
        ]
  arg <- toJSVal e
  call f f [arg]
  return ()

frontend :: Frontend (R FrontendRoute)
frontend = Frontend
  { _frontend_head = do
    el "title" $ text "Obelisk Minimal Example"
    elAttr "meta" ("charset" =: "utf-8") blank
    elAttr "script" ("type" =: "text/javascript" <> "src" =: static @"echarts.min.js") blank
  , _frontend_body = prerender blank echarts
  }

echarts
  :: ( DomBuilder t m
     , PostBuild t m
     , PerformEvent t m
     , MonadJSM (Performable m)
     -- , TriggerEvent t m
     , GhcjsDomSpace ~ DomBuilderSpace m
     )
  => m ()
echarts = el "main" $ do
  (e, _) <- elAttr' "div" ("style" =: "width:600px;height:400px;") blank
  p <- getPostBuild
  performEvent_ $ ffor p $ \_ -> liftJSM $ example $ _element_raw e
  return ()

