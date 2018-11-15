{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
module Frontend where

import Control.Monad (join, void)
import Control.Monad.Fix (MonadFix)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (ToJSON, genericToEncoding, genericToJSON, defaultOptions, Options(..))
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LBS
import Data.Default (Default, def)
import qualified Data.HashMap.Strict as HashMap
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Scientific
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time
import Data.Time.ISO8601
import qualified Data.Vector as V
import GHC.Generics (Generic)
import GHCJS.DOM.Types (Element)
import JSDOM.Types (JSVal, toJSVal, JSM, MonadJSM, liftJSM)
import Language.Javascript.JSaddle.Evaluate
import Language.Javascript.JSaddle.Object
import Network.URI (parseURI, URI(..), URIAuth(..))
import qualified Obelisk.ExecutableConfig
import Obelisk.Frontend
import Obelisk.Route
import Reflex.Dom.Core

-- import Common.Api
import Common.Route
import Obelisk.Generated.Static

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

instance ToJSON Align where
  toJSON a = Aeson.String $ alignToText a

data VerticalAlign = VerticalAlign_Auto
                   | VerticalAlign_Top
                   | VerticalAlign_Middle
                   | VerticalAlign_Bottom

instance ToJSON VerticalAlign where
  toJSON VerticalAlign_Auto = Aeson.String "auto"
  toJSON VerticalAlign_Top = Aeson.String "top"
  toJSON VerticalAlign_Middle = Aeson.String "middle"
  toJSON VerticalAlign_Bottom = Aeson.String "bottom"

data SizeValue = SizeValue_Auto
               | SizeValue_Percent Int
               | SizeValue_Numeric Int

sizeValueToSN :: SizeValue -> SN
sizeValueToSN = \case
  SizeValue_Auto -> SN_String "auto"
  SizeValue_Percent n -> SN_String $ T.pack (show n) <> "%"
  SizeValue_Numeric n -> SN_Number $ fromIntegral n

data TextStyle = TextStyle
  { _textStyle_color :: Maybe Text
  , _textStyle_font :: Maybe Font
  , _textStyle_align :: Maybe Align
  , _textStyle_verticalAlign :: Maybe VerticalAlign
  , _textStyle_lineHeight :: Maybe Int
  , _textStyle_height :: Maybe SizeValue
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

posAlignToSN :: PosAlign -> SN
posAlignToSN = \case
  PosAlign_Auto -> SN_String "auto"
  PosAlign_Pixel n -> SN_Number $ fromIntegral n
  PosAlign_Percent n -> SN_String $ T.pack (show n) <> "%"
  PosAlign_Align a -> SN_String $ alignToText a

alignToText :: Align -> Text
alignToText = \case
  Align_Auto -> "auto"
  Align_Center -> "center"
  Align_Left -> "left"
  Align_Right -> "right"

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

instance ToJSON Orientation where
  toJSON = \case
    Orientation_Auto -> Aeson.String "auto"
    Orientation_Horizontal -> Aeson.String "horizontal"
    Orientation_Vertical -> Aeson.String "vertical"

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

instance Default Title where
  def = Title
    { _title_show = Nothing
    , _title_text = Nothing
    , _title_link = Nothing
    , _title_target = Nothing
    , _title_textStyle = Nothing
    , _title_subtext = Nothing
    , _title_sublink = Nothing
    , _title_subtarget = Nothing
    , _title_subtextStyle = Nothing
    , _title_triggerEvent = Nothing
    , _title_padding = Nothing
    , _title_itemGap = Nothing
    , _title_backgroundColor = Nothing
    , _title_border = Nothing
    , _title_shadow = Nothing
    , _title_position = Nothing
    }

data LegendType = LegendType_Plain
                | LegendType_Scroll

instance ToJSON LegendType where
  toJSON LegendType_Plain = Aeson.String "plain"
  toJSON LegendType_Scroll = Aeson.String "scroll"

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

instance ToJSON Icon where
  toJSON = Aeson.String . \case
    Icon_Circle -> "circle"
    Icon_Rect -> "rect"
    Icon_RoundRect -> "roundRect"
    Icon_Triangle -> "triangle"
    Icon_Diamond -> "diamond"
    Icon_Pin -> "pin"
    Icon_Arrow -> "arrow"
    Icon_None -> "none"
    Icon_Image url -> "image://" <> url
    Icon_DataURI uri -> "image://" <> uri
    Icon_SVGPath svg -> "path://" <> svg


data LegendData = LegendData
  { _legendData_name :: Maybe Text
  , _legendData_icon :: Maybe Icon
  , _legendData_textStyle :: Maybe TextStyle
  }

instance Default LegendData where
  def = LegendData Nothing Nothing Nothing

data PageButtonPosition = PageButtonPosition_Start
                        | PageButtonPosition_End

instance ToJSON PageButtonPosition where
  toJSON = Aeson.String . \case
    PageButtonPosition_Start -> "start"
    PageButtonPosition_End -> "end"

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
  , _legend_backgroundColor :: Maybe Text
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

instance Default Legend where
  def = Legend
    { _legend_type = Nothing
    , _legend_show = Nothing
    , _legend_position = Nothing
    , _legend_size = Nothing
    , _legend_orient = Nothing
    , _legend_align = Nothing
    , _legend_padding = Nothing
    , _legend_itemGap = Nothing
    , _legend_itemWidth = Nothing
    , _legend_itemHeight = Nothing
    , _legend_symbolKeepAspect = Nothing
    , _legend_formatter = Nothing
    , _legend_selectedMode = Nothing
    , _legend_inactiveColor = Nothing
    , _legend_selected = Nothing
    , _legend_textStyle = Nothing
    , _legend_data = Nothing
    , _legend_backgroundColor = Nothing
    , _legend_border = Nothing
    , _legend_shadow = Nothing
    , _legend_scrollDataIndex = Nothing
    , _legend_pageButtonItemGap = Nothing
    , _legend_pageButtonGap = Nothing
    , _legend_pageButtonPosition = Nothing
    , _legend_pageFormatter = Nothing
    , _legend_pageTextStyle = Nothing
    , _legend_animation = Nothing
    , _legend_animationDurationUpdate = Nothing
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

instance ToJSON AxisPosition where
  toJSON = Aeson.String . \case
    AxisPosition_Top -> "top"
    AxisPosition_Bottom -> "bottom"

data AxisType = AxisType_Value
               | AxisType_Category
               | AxisType_Time
               | AxisType_Log

instance ToJSON AxisType where
  toJSON = Aeson.String . \case
    AxisType_Value -> "value"
    AxisType_Category -> "category"
    AxisType_Time -> "time"
    AxisType_Log -> "log"

data AxisNameLocation = AxisNameLocation_Start
                      | AxisNameLocation_Center
                      | AxisNameLocation_End

instance ToJSON AxisNameLocation where
  toJSON = Aeson.String . \case
    AxisNameLocation_Start -> "start"
    AxisNameLocation_Center -> "center"
    AxisNameLocation_End -> "end"

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
  , _axis_min :: Maybe (Either Double ())
  , _axis_max :: Maybe (Either Double ())
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

instance Default Axis where
  def = Axis
    { _axis_show = Nothing
    , _axis_gridIndex = Nothing
    , _axis_position = Nothing
    , _axis_offset = Nothing
    , _axis_type = Nothing
    , _axis_name = Nothing
    , _axis_nameLocation = Nothing
    , _axis_nameTextStyle = Nothing
    , _axis_nameGap = Nothing
    , _axis_nameRotate = Nothing
    , _axis_inverse = Nothing
    , _axis_boundaryGap = Nothing
    , _axis_min = Nothing
    , _axis_max = Nothing
    , _axis_scale = Nothing
    , _axis_minInterval = Nothing
    , _axis_interval = Nothing
    , _axis_logBase = Nothing
    , _axis_silent = Nothing
    , _axis_triggerEvent = Nothing
    , _axis_axisLine = Nothing
    , _axis_axisTick = Nothing
    , _axis_axisLabel = Nothing
    , _axis_data = Nothing
    , _axis_zlevel = Nothing
    , _axis_z = Nothing
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
      , _seriesLine_data :: Maybe [(Scientific, Scientific)]
      , _seriesLine_smooth :: Maybe Bool
      }
  | Series_Timeline
      { _seriesTimeline_name :: Maybe Text
      , _seriesTimeline_timeAxisX :: Bool
      , _seriesTimeline_data :: Maybe [(UTCTime, Scientific)]
      , _seriesTimeline_smooth :: Maybe Bool
      }

data ChartOptions = ChartOptions
  { _chartOptions_title :: Title
  , _chartOptions_legend :: Legend
  -- , _chartOptions_grid :: Grid
  , _chartOptions_xAxis :: Axis
  , _chartOptions_yAxis :: Axis
  , _chartOptions_series :: [Series]
  }

instance Default ChartOptions where
  def = ChartOptions def def def def []

data EChartTitle = EChartTitle
  { _eChartTitle_show :: Maybe Bool
  , _eChartTitle_text :: Maybe Text
  , _eChartTitle_link :: Maybe Text
  , _eChartTitle_target :: Maybe Target
  , _eChartTitle_textStyle :: Maybe EChartTextStyle
  , _eChartTitle_subtext :: Maybe Text
  , _eChartTitle_sublink :: Maybe Text
  , _eChartTitle_subtarget :: Maybe Target
  , _eChartTitle_subtextStyle :: Maybe EChartTextStyle
  , _eChartTitle_triggerEvent :: Maybe Bool
  , _eChartTitle_padding :: Maybe (Int, Int, Int, Int)
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
  , _eChartTitle_borderRadius :: Maybe (Int, Int, Int, Int)
  , _eChartTitle_shadowBlur :: Maybe Int
  , _eChartTitle_shadowColor :: Maybe Text
  , _eChartTitle_shadowOffsetX :: Maybe Int
  , _eChartTitle_shadowOffsetY :: Maybe Int
  }
  deriving (Generic)

instance ToJSON EChartTitle where
  toJSON = genericToJSON $ defaultOptions
    { fieldLabelModifier = drop $ T.length "_eChartTitle_"
    , omitNothingFields = True
    }
  toEncoding = genericToEncoding $ defaultOptions
    { fieldLabelModifier = drop $ T.length "_eChartTitle_"
    , omitNothingFields = True
    }

data EChartTextStyle = EChartTextStyle
  { _eChartTextStyle_color :: Maybe Text
  , _eChartTextStyle_fontStyle :: Maybe FontStyle
  , _eChartTextStyle_fontWeight :: Maybe FontWeight
  , _eChartTextStyle_fontFamily :: Maybe FontFamily
  , _eChartTextStyle_fontSize :: Maybe Int
  , _eChartTextStyle_align :: Maybe Align
  , _eChartTextStyle_verticalAlign :: Maybe VerticalAlign
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
  toJSON = genericToJSON $ defaultOptions
    { fieldLabelModifier = drop $ T.length "_eChartTextStyle_"
    , omitNothingFields = True
    }
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
  { _eChartLegend_type :: Maybe LegendType
  , _eChartLegend_show :: Maybe Bool
  , _eChartLegend_zlevel :: Maybe Int
  , _eChartLegend_z :: Maybe Int
  , _eChartLegend_left :: Maybe SN
  , _eChartLegend_top :: Maybe SN
  , _eChartLegend_right :: Maybe SN
  , _eChartLegend_bottom :: Maybe SN
  , _eChartLegend_width :: Maybe SN
  , _eChartLegend_height :: Maybe SN
  , _eChartLegend_orient :: Maybe Orientation
  , _eChartLegend_align :: Maybe Align
  , _eChartLegend_padding :: Maybe (Int, Int, Int, Int)
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
  , _eChartLegend_backgroundColor :: Maybe Text
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
  , _eChartLegend_pageButtonPosition :: Maybe PageButtonPosition
  , _eChartLegend_pageFormatter :: Maybe Text
  , _eChartLegend_pageTextStyle :: Maybe EChartTextStyle
  , _eChartLegend_animation :: Maybe Bool
  , _eChartLegend_animationDurationUpdate :: Maybe Int
  }
  deriving (Generic)

instance ToJSON EChartLegend where
  toJSON = genericToJSON $ defaultOptions
    { fieldLabelModifier = drop $ T.length "_eChartLegend_"
    , omitNothingFields = True
    }
  toEncoding = genericToEncoding $ defaultOptions
    { fieldLabelModifier = drop $ T.length "_eChartLegend_"
    , omitNothingFields = True
    }

data EChartAxis = EChartAxis
  { _eChartAxis_show :: Maybe Bool
  , _eChartAxis_gridIndex :: Maybe Int
  , _eChartAxis_position :: Maybe AxisPosition
  , _eChartAxis_offset :: Maybe Int
  , _eChartAxis_type :: Maybe AxisType
  , _eChartAxis_name :: Maybe Text
  , _eChartAxis_nameLocation :: Maybe AxisNameLocation
  , _eChartAxis_nameTextStyle :: Maybe EChartTextStyle
  , _eChartAxis_nameGap :: Maybe Int
  , _eChartAxis_nameRotate :: Maybe Int
  , _eChartAxis_inverse :: Maybe Bool
  , _eChartAxis_boundaryGap :: Maybe Aeson.Value
  , _eChartAxis_min :: Maybe SN
  , _eChartAxis_max :: Maybe SN
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
  toJSON = genericToJSON $ defaultOptions
    { fieldLabelModifier = drop $ T.length "_eChartAxis_"
    , omitNothingFields = True
    }
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
  toJSON = genericToJSON $ defaultOptions
    { fieldLabelModifier = drop $ T.length "_eChartAxisLine_"
    , omitNothingFields = True
    }
  toEncoding = genericToEncoding $ defaultOptions
    { fieldLabelModifier = drop $ T.length "_eChartAxisLine_"
    , omitNothingFields = True
    }

data EChartLineStyle = EChartLineStyle
  { _eChartLineStyle_color :: Maybe Text
  , _eChartLineStyle_width :: Maybe Int
  , _eChartLineStyle_type :: Maybe LineStyleType
  , _eChartLineStyle_opacity :: Maybe Double
  , _eChartLineStyle_shadowColor :: Maybe Text
  , _eChartLineStyle_shadowBlur :: Maybe Int
  , _eChartLineStyle_shadowOffsetX :: Maybe Int
  , _eChartLineStyle_shadowOffsetY :: Maybe Int
  }
  deriving (Generic)

instance ToJSON EChartLineStyle where
  toJSON = genericToJSON $ defaultOptions
    { fieldLabelModifier = drop $ T.length "_eChartLineStyle_"
    , omitNothingFields = True
    }
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
  toJSON = genericToJSON $ defaultOptions
    { fieldLabelModifier = drop $ T.length "_eChartAxisTick_"
    , omitNothingFields = True
    }
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
  , _eChartAxisLabel_align :: Maybe Align
  , _eChartAxisLabel_verticalAlign :: Maybe VerticalAlign
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
  toJSON = genericToJSON $ defaultOptions
    { fieldLabelModifier = drop $ T.length "_eChartAxisLabel_"
    , omitNothingFields = True
    }
  toEncoding = genericToEncoding $ defaultOptions
    { fieldLabelModifier = drop $ T.length "_eChartAxisLabel_"
    , omitNothingFields = True
    }

data EChartSeries = EChartSeries
  { _eChartSeries_type :: Maybe Text
  , _eChartSeries_name :: Maybe Text
  , _eChartSeries_data :: Maybe Aeson.Value
  , _eChartSeries_smooth :: Maybe Bool
  , _eChartSeries_animation :: Maybe Bool
  }
  deriving (Generic)

instance ToJSON EChartSeries where
  toJSON = genericToJSON $ defaultOptions
    { fieldLabelModifier = drop $ T.length "_eChartSeries_"
    , omitNothingFields = True
    }
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
  toJSON = genericToJSON (defaultOptions
    { fieldLabelModifier = drop (T.length "_eChartConfig_")
    , omitNothingFields = True
    })
  toEncoding = genericToEncoding (defaultOptions
    { fieldLabelModifier = drop (T.length "_eChartConfig_")
    , omitNothingFields = True
    })

toEChartConfig :: ChartOptions -> EChartConfig
toEChartConfig c = EChartConfig
  { _eChartConfig_title = toEChartTitle $ _chartOptions_title c
  , _eChartConfig_legend = toEChartLegend $ _chartOptions_legend c
  -- , _eChartConfig_grid = toEChartGrid $ _chartOptions_grid c
  , _eChartConfig_xAxis = toEChartAxis $ _chartOptions_xAxis c
  , _eChartConfig_yAxis = toEChartAxis $ _chartOptions_yAxis c
  , _eChartConfig_series = fmap toEChartSeries $ _chartOptions_series c
  }
  where
    toEChartTextStyle :: TextStyle -> EChartTextStyle
    toEChartTextStyle t = EChartTextStyle
      { _eChartTextStyle_color = _textStyle_color t
      , _eChartTextStyle_fontStyle = join $ fmap _font_style $ _textStyle_font t
      , _eChartTextStyle_fontWeight = join $ fmap _font_weight $ _textStyle_font t
      , _eChartTextStyle_fontFamily = join $ fmap _font_family $ _textStyle_font t
      , _eChartTextStyle_fontSize = join $ fmap _font_size $ _textStyle_font t
      , _eChartTextStyle_align = _textStyle_align t
      , _eChartTextStyle_verticalAlign = _textStyle_verticalAlign t
      , _eChartTextStyle_lineHeight = _textStyle_lineHeight t
      , _eChartTextStyle_width = fmap sizeValueToSN $ _textStyle_width t
      , _eChartTextStyle_height = fmap sizeValueToSN $ _textStyle_height t
      , _eChartTextStyle_textBorderColor = join $ fmap _border_color $ _textStyle_textBorder t
      , _eChartTextStyle_textBorderWidth = join $ fmap _border_width $ _textStyle_textBorder t
      , _eChartTextStyle_textShadowColor = join $ fmap _shadow_color $ _textStyle_textShadow t
      , _eChartTextStyle_textShadowBlur = join $ fmap _shadow_blur $ _textStyle_textShadow t
      , _eChartTextStyle_textShadowOffsetX = join $ fmap _shadow_offsetX $ _textStyle_textShadow t
      , _eChartTextStyle_textShadowOffsetY = join $ fmap _shadow_offsetY $ _textStyle_textShadow t
      }
    toEChartTitle :: Title -> EChartTitle
    toEChartTitle t = EChartTitle
      { _eChartTitle_show = _title_show t
      , _eChartTitle_text = _title_text t
      , _eChartTitle_link = _title_link t
      , _eChartTitle_target = _title_target t
      , _eChartTitle_textStyle = fmap toEChartTextStyle $ _title_textStyle t
      , _eChartTitle_subtext = _title_subtext t
      , _eChartTitle_sublink = _title_sublink t
      , _eChartTitle_subtarget = _title_subtarget t
      , _eChartTitle_subtextStyle = fmap toEChartTextStyle $ _title_subtextStyle t
      , _eChartTitle_triggerEvent = _title_triggerEvent t
      , _eChartTitle_padding = _title_padding t
      , _eChartTitle_itemGap = _title_itemGap t
      , _eChartTitle_zlevel = join $ fmap _position_zlevel $ _title_position t
      , _eChartTitle_z = join $ fmap _position_z $ _title_position t
      , _eChartTitle_left = fmap posAlignToSN $ join $ fmap _position_left $ _title_position t
      , _eChartTitle_right = fmap posAlignToSN $ join $ fmap _position_right $ _title_position t
      , _eChartTitle_top = fmap posAlignToSN $ join $ fmap _position_top $ _title_position t
      , _eChartTitle_bottom = fmap posAlignToSN $ join $ fmap _position_bottom $ _title_position t
      , _eChartTitle_backgroundColor = _title_backgroundColor t
      , _eChartTitle_borderColor = join $ fmap _border_color $ _title_border t
      , _eChartTitle_borderWidth = join $ fmap _border_width $ _title_border t
      , _eChartTitle_borderRadius = join $ fmap _border_radius $ _title_border t
      , _eChartTitle_shadowBlur = join $ fmap _shadow_blur $ _title_shadow t
      , _eChartTitle_shadowColor = join $ fmap _shadow_color $ _title_shadow t
      , _eChartTitle_shadowOffsetX = join $ fmap _shadow_offsetX $ _title_shadow t
      , _eChartTitle_shadowOffsetY = join $ fmap _shadow_offsetY $ _title_shadow t
      }
    fromPos a b = fmap posAlignToSN $ join $ fmap a b
    toEChartLegend x = EChartLegend
      { _eChartLegend_type = _legend_type x
      , _eChartLegend_show = _legend_show x
      , _eChartLegend_zlevel = join $ fmap _position_zlevel $ _legend_position x
      , _eChartLegend_z = join $ fmap _position_z $ _legend_position x
      , _eChartLegend_left = fromPos _position_left $ _legend_position x
      , _eChartLegend_top = fromPos _position_top $ _legend_position x
      , _eChartLegend_right = fromPos _position_right $ _legend_position x
      , _eChartLegend_bottom = fromPos _position_bottom $ _legend_position x
      , _eChartLegend_width = fmap sizeValueToSN $ join $ fmap _size_width $ _legend_size x
      , _eChartLegend_height = fmap sizeValueToSN $ join $ fmap _size_height $ _legend_size x
      , _eChartLegend_orient = _legend_orient x
      , _eChartLegend_align = _legend_align x
      , _eChartLegend_padding = _legend_padding x
      , _eChartLegend_itemGap = _legend_itemGap x
      , _eChartLegend_itemWidth = _legend_itemWidth x
      , _eChartLegend_itemHeight = _legend_itemHeight x
      , _eChartLegend_symbolKeepAspect = _legend_symbolKeepAspect x
      , _eChartLegend_formatter = _legend_formatter x
      , _eChartLegend_selectedMode = _legend_selectedMode x
      , _eChartLegend_inactiveColor = _legend_inactiveColor x
      , _eChartLegend_selected =
        flip fmap (_legend_selected x) $ \s -> Aeson.Object $
          HashMap.fromList $ fmap (\(k, v) -> (k, Aeson.toJSON v)) $ Map.toList s
      , _eChartLegend_textStyle = fmap toEChartTextStyle $ _legend_textStyle x
      , _eChartLegend_data =
        let toLegendDataObject (k, v) = Aeson.Object $ HashMap.mapMaybe id $ HashMap.fromList
              [ ("name", Just $ Aeson.toJSON k)
              , ("icon", fmap Aeson.toJSON $ _legendData_icon v)
              , ("textStyle", Aeson.toJSON . toEChartTextStyle <$> _legendData_textStyle v)
              ]
        in  flip fmap (_legend_data x) $ \d ->
              Aeson.Array $ fmap toLegendDataObject $ V.fromList $ Map.toList d
      , _eChartLegend_backgroundColor = _legend_backgroundColor x
      , _eChartLegend_borderColor = join $ fmap _border_color $ _legend_border x
      , _eChartLegend_borderWidth = join $ fmap _border_width $ _legend_border x
      , _eChartLegend_borderRadius = join $ fmap _border_radius $ _legend_border x
      , _eChartLegend_shadowBlur = join $ fmap _shadow_blur $ _legend_shadow x
      , _eChartLegend_shadowColor = join $ fmap _shadow_color $ _legend_shadow x
      , _eChartLegend_shadowOffsetX = join $ fmap _shadow_offsetX $ _legend_shadow x
      , _eChartLegend_shadowOffsetY = join $ fmap _shadow_offsetY $ _legend_shadow x
      , _eChartLegend_scrollDataIndex = _legend_scrollDataIndex x
      , _eChartLegend_pageButtonItemGap = _legend_pageButtonItemGap x
      , _eChartLegend_pageButtonGap = _legend_pageButtonGap x
      , _eChartLegend_pageButtonPosition = _legend_pageButtonPosition x
      , _eChartLegend_pageFormatter = _legend_pageFormatter x
      , _eChartLegend_pageTextStyle = fmap toEChartTextStyle $ _legend_pageTextStyle x
      , _eChartLegend_animation = _legend_animation x
      , _eChartLegend_animationDurationUpdate = _legend_animationDurationUpdate x
      }
    toEChartAxis :: Axis -> EChartAxis
    toEChartAxis x = EChartAxis
      { _eChartAxis_show = _axis_show x
      , _eChartAxis_gridIndex = _axis_gridIndex x
      , _eChartAxis_zlevel = _axis_zlevel x
      , _eChartAxis_z = _axis_z x
      , _eChartAxis_offset = _axis_offset x
      , _eChartAxis_type = _axis_type x
      , _eChartAxis_name = _axis_name x
      , _eChartAxis_nameLocation = _axis_nameLocation x
      , _eChartAxis_nameTextStyle = fmap toEChartTextStyle $ _axis_nameTextStyle x
      , _eChartAxis_nameGap = _axis_nameGap x
      , _eChartAxis_nameRotate = _axis_nameRotate x
      , _eChartAxis_inverse = _axis_inverse x
      , _eChartAxis_boundaryGap = case _axis_boundaryGap x of
          Nothing -> Nothing
          Just (Left gap) -> Just $ Aeson.Bool gap
          Just (Right (a, b)) -> Just $ Aeson.toJSON (sizeValueToSN a, sizeValueToSN b)
      , _eChartAxis_min = ffor (_axis_min x) $ \case
          Right () -> SN_String "dataMin"
          Left n -> SN_Number n
      , _eChartAxis_max = ffor (_axis_max x) $ \case
          Right () -> SN_String "dataMax"
          Left n -> SN_Number n
      , _eChartAxis_scale = _axis_scale x
      , _eChartAxis_minInterval = _axis_minInterval x
      , _eChartAxis_interval = _axis_interval x
      , _eChartAxis_logBase = _axis_logBase x
      , _eChartAxis_silent = _axis_silent x
      , _eChartAxis_triggerEvent = _axis_triggerEvent x
      , _eChartAxis_axisLine = fmap toEChartAxisLine $ _axis_axisLine x
      , _eChartAxis_axisTick = fmap toEChartAxisTick $ _axis_axisTick x
      , _eChartAxis_axisLabel = fmap toEChartAxisLabel $ _axis_axisLabel x
      , _eChartAxis_position = _axis_position x
      , _eChartAxis_data = case _axis_data x of
          Nothing -> Nothing
          Just d -> Just $ Aeson.Array $ V.fromList $
            fmap (\(k, v) -> Aeson.Object $ HashMap.fromList
              [ ("value", Aeson.toJSON k)
              , ("textStyle", Aeson.toJSON $ fmap toEChartTextStyle v)
              ]) $ Map.toList d
      }
    toEChartAxisLabel x = EChartAxisLabel
      { _eChartAxisLabel_show = _axisLabel_show x
      , _eChartAxisLabel_inside = _axisLabel_inside x
      , _eChartAxisLabel_rotate = _axisLabel_rotate x
      , _eChartAxisLabel_margin = _axisLabel_margin x
      , _eChartAxisLabel_showMinLabel = _axisLabel_showMinLabel x
      , _eChartAxisLabel_showMaxLabel = _axisLabel_showMaxLabel x
      , _eChartAxisLabel_fontStyle = join $ fmap _font_style $ _axisLabel_font x
      , _eChartAxisLabel_fontWeight = join $ fmap _font_weight $ _axisLabel_font x
      , _eChartAxisLabel_fontFamily = join $ fmap _font_family $ _axisLabel_font x
      , _eChartAxisLabel_fontSize = join $ fmap _font_size $ _axisLabel_font x
      , _eChartAxisLabel_align = _axisLabel_align x
      , _eChartAxisLabel_verticalAlign = _axisLabel_verticalAlign x
      , _eChartAxisLabel_lineHeight = _axisLabel_lineHeight x
      , _eChartAxisLabel_backgroundColor = _axisLabel_backgroundColor x
      , _eChartAxisLabel_borderColor = join $ fmap _border_color $ _axisLabel_border x
      , _eChartAxisLabel_borderWidth = join $ fmap _border_width $ _axisLabel_border x
      , _eChartAxisLabel_borderRadius = join $ fmap _border_radius $ _axisLabel_border x
      , _eChartAxisLabel_shadowBlur = join $ fmap _shadow_blur $ _axisLabel_shadow x
      , _eChartAxisLabel_shadowColor = join $ fmap _shadow_color $ _axisLabel_shadow x
      , _eChartAxisLabel_shadowOffsetX = join $ fmap _shadow_offsetX $ _axisLabel_shadow x
      , _eChartAxisLabel_shadowOffsetY = join $ fmap _shadow_offsetY $ _axisLabel_shadow x
      , _eChartAxisLabel_padding = _axisLabel_padding x
      , _eChartAxisLabel_width = fmap sizeValueToSN $ join $ fmap _size_width $ _axisLabel_size x
      , _eChartAxisLabel_height = fmap sizeValueToSN $ join $ fmap _size_height $ _axisLabel_size x
      , _eChartAxisLabel_textBorderColor = join $ fmap _border_color $ _axisLabel_textBorder x
      , _eChartAxisLabel_textBorderWidth = join $ fmap _border_width $ _axisLabel_textBorder x
      , _eChartAxisLabel_textBorderRadius = join $ fmap _border_radius $ _axisLabel_textBorder x
      , _eChartAxisLabel_textShadowColor = join $ fmap _shadow_color $ _axisLabel_textShadow x
      , _eChartAxisLabel_textShadowBlur = join $ fmap _shadow_blur $ _axisLabel_textShadow x
      , _eChartAxisLabel_textShadowOffsetX = join $ fmap _shadow_offsetX $ _axisLabel_textShadow x
      , _eChartAxisLabel_textShadowOffsetY = join $ fmap _shadow_offsetY $ _axisLabel_textShadow x
      }
    toEChartAxisTick x = EChartAxisTick
      { _eChartAxisTick_show = _axisTick_show x
      , _eChartAxisTick_alignWithLabel = _axisTick_alignWithLabel x
      , _eChartAxisTick_inside = _axisTick_inside x
      , _eChartAxisTick_length = _axisTick_length x
      , _eChartAxisTick_lineStyle = fmap toEChartLineStyle $ _axisTick_lineStyle x
      }
    toEChartAxisLine a = EChartAxisLine
      { _eChartAxisLine_onZero = _axisLine_onZero a
      , _eChartAxisLine_onZeroAxisIndex = _axisLine_onZeroAxisIndex a
      , _eChartAxisLine_show = _axisLine_show a
      , _eChartAxisLine_symbol = _axisLine_symbol a
      , _eChartAxisLine_symbolOffset = _axisLine_symbolOffset a
      , _eChartAxisLine_lineStyle = fmap toEChartLineStyle $ _axisLine_lineStyle a
      , _eChartAxisLine_symbolSize = _axisLine_symbolSize a
      }
    toEChartLineStyle x = EChartLineStyle
      { _eChartLineStyle_color = _lineStyle_color x
      , _eChartLineStyle_width = _lineStyle_width x
      , _eChartLineStyle_type = _lineStyle_type x
      , _eChartLineStyle_opacity = _lineStyle_opacity x
      , _eChartLineStyle_shadowColor = join $ fmap _shadow_color $ _lineStyle_shadow  x
      , _eChartLineStyle_shadowBlur = join $ fmap _shadow_blur $ _lineStyle_shadow x
      , _eChartLineStyle_shadowOffsetX = join $ fmap _shadow_offsetX $ _lineStyle_shadow x
      , _eChartLineStyle_shadowOffsetY = join $ fmap _shadow_offsetY $ _lineStyle_shadow x
      }
    toEChartSeries x = case x of
      Series_Line n d smooth ->
        let d' = case d of
              Nothing -> Nothing
              Just xs -> Just $ Aeson.Array $ V.fromList $ fmap Aeson.toJSON xs
        in  EChartSeries (Just "line") n d' smooth Nothing
      Series_Timeline n timeX d smooth ->
        let d' = case d of
              Nothing -> Nothing
              Just xs -> Just $ Aeson.Array $ V.fromList $
                -- Note: If more than three digits of precision is provided for
                -- the seconds, echarts interprets that as a larger number of
                -- milliseconds, not more precision.  E.g.: "0.123456" seconds
                -- is interpreted as 123.456 seconds
                ffor xs $ \(t, v) -> Aeson.Object $ HashMap.fromList
                  [ ("name", Aeson.String $ T.pack $ show t)
                  , ("value", (if timeX then Aeson.toJSON else Aeson.toJSON . swap) (formatISO8601Millis t, v))
                  ]
        in EChartSeries (Just "line") n d' smooth Nothing
    swap (x, y) = (y, x)

data ECharts = ECharts { unECharts :: JSVal }

init :: GHCJS.DOM.Types.Element -> JSM ECharts
init e = do
  f <- eval $ T.pack "(function(e) { return echarts['init'](e) })"
  arg <- toJSVal e
  ECharts <$> call f f [arg]

setOption :: ECharts -> ChartOptions -> JSM ()
setOption c opts = do
  f <- eval $ T.pack "(function(e, opt) { e['setOption'](opt); })"
  let chart = unECharts c
  options <- toJSVal $ Aeson.toJSON $ toEChartConfig opts
  void $ call f f [chart, options]

frontend :: Frontend (R FrontendRoute)
frontend = Frontend
  { _frontend_head = do
    el "title" $ text "Obelisk Minimal Example"
    elAttr "meta" ("charset" =: "utf-8") blank
    elAttr "script" ("type" =: "text/javascript" <> "src" =: static @"echarts.min.js") blank
  , _frontend_body = do
    Just r <- liftIO $ Obelisk.ExecutableConfig.get "config/common/route"
    let Just (URI scheme (Just auth)  _ _ _) = parseURI $ T.unpack $ T.strip r
        wsScheme = case scheme of
          "https:" -> "wss:"
          _ -> "ws:"
        wsUrl = T.pack $ wsScheme <> (uriRegName auth) <> (uriPort auth) <> "/listen"
    prerender blank (echarts wsUrl)
  }

echarts
  :: forall t m.
     ( DomBuilder t m
     , PostBuild t m
     , PerformEvent t m
     , MonadJSM (Performable m)
     , GhcjsDomSpace ~ DomBuilderSpace m
     , MonadHold t m
     , MonadFix m
     , TriggerEvent t m
     , HasJSContext m
     , MonadJSM m
     )
  => Text
  -> m ()
echarts wsUrl = el "main" $ do
  ws <- webSocket wsUrl $ def & webSocketConfig_send .~ (never :: Event t [Text])
  receivedMessages :: Dynamic t [(UTCTime, Scientific)] <- foldDyn (\m ms -> case Aeson.decode (LBS.fromStrict m) of
    Nothing -> ms
    Just m' -> take 50 $ m' : ms) [] $ _webSocket_recv ws
  dynamicTimeSeries "Test" $ Map.singleton "user" . reverse <$> receivedMessages


dynamicTimeSeries
  :: ( DomBuilder t m
     , PerformEvent t m
     , PostBuild t m
     , MonadHold t m
     , MonadJSM (Performable m)
     , GhcjsDomSpace ~ DomBuilderSpace m
     )
  => Text
  -> Dynamic t (Map Text [(UTCTime, Scientific)])
  -> m ()
dynamicTimeSeries title ts = do
  e <- fst <$> elAttr' "div" ("style" =: "width:600px; height:400px;") blank
  p <- getPostBuild
  chart <- performEvent $ ffor p $ \_ -> liftJSM $ Frontend.init $ _element_raw e
  let opts0 = def
        { _chartOptions_title = def { _title_text = Just title }
        , _chartOptions_xAxis = def { _axis_type = Just AxisType_Time }
        , _chartOptions_yAxis = def { _axis_type = Just AxisType_Value
                                    , _axis_min = Just $ Left 0
                                    , _axis_max = Just $ Left 1600
                                    }
        , _chartOptions_series = []
        }
  performEvent_ $ ffor chart $ \c -> liftJSM $ setOption c opts0
  mchart <- holdDyn Nothing $ Just <$> chart
  let opts = leftmost
        [ attach (current mchart) $ updated ts
        , attachWith (\t c -> (c, t)) (current ts) (updated mchart)
        ]
  performEvent_ $ fforMaybe opts $ \case
    (Nothing, _) -> Nothing
    (Just c, ts') -> Just $ liftJSM $ setOption c $ opts0
      { _chartOptions_series = ffor (Map.toList ts') $ \(k, vs) ->
        Series_Timeline (Just k) True (Just vs) (Just True)
      }
