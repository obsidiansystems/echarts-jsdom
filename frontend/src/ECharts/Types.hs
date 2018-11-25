{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
module ECharts.Types where

import Data.Aeson (ToJSON, genericToEncoding, genericToJSON, defaultOptions, Options(..))
import Data.Default (Default, def)
import qualified Data.Aeson as Aeson
import Data.Text (Text)
import qualified Data.Text as T
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Scientific
import Data.Time
import Data.Time.Clock.POSIX
import GHC.Generics (Generic)

type ZeroToOne = Scientific
type CoordinateSystem = Aeson.Value
type Symbol = Aeson.Value
type SymbolSize = Aeson.Value
type Step = Aeson.Value
type Label = Aeson.Value
type ItemStyle = Aeson.Value
type AreaStyle = Aeson.Value
type Emphasis = Aeson.Value
type SmoothMonotone = Aeson.Value
type Sampling = Aeson.Value
type Encode = Aeson.Value
type MarkPoint = Aeson.Value
type MarkLine = Aeson.Value
type MarkArea = Aeson.Value
type Animation = Aeson.Value
type ToolTip = Aeson.Value
type SelectedMode = Aeson.Value
type AbsOrPercentage = Aeson.Value
type RippleEffect = Aeson.Value
type TextOrScientific = Aeson.Value

utcTimeToEpoch :: UTCTime -> Int
utcTimeToEpoch t = round $ (utcTimeToPOSIXSeconds t * 1000)

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
  deriving (Generic)

instance ToJSON Shadow where
  toJSON = genericToJSON $ defaultOptions
    { fieldLabelModifier = drop $ T.length "_shadow_"
    , omitNothingFields = True
    }
  toEncoding = genericToEncoding $ defaultOptions
    { fieldLabelModifier = drop $ T.length "_shadow_"
    , omitNothingFields = True
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
  , _axis_data :: Maybe [(Text, (Maybe TextStyle))]
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
  deriving (Generic)

instance ToJSON LineStyle where
  toJSON = genericToJSON $ defaultOptions
    { fieldLabelModifier = drop $ T.length "_lineStyle_"
    , omitNothingFields = True
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


data SN = SN_String Text
        | SN_Number Double
        deriving (Generic)

instance ToJSON SN where
  toJSON (SN_String a) = Aeson.String a
  toJSON (SN_Number a) = Aeson.Number $ realToFrac a

data SeriesLine
data SeriesBar
data SeriesPie
data SeriesScatter
data SeriesEffectScatter
data SeriesRadar
data SeriesTree
data SeriesTreemap
data SeriesSunburst
data SeriesBoxplot
data SeriesCandlestick
data SeriesHeatmap
data SeriesMap
data SeriesParallel
data SeriesLines
data SeriesGraph
data SeriesSankey
data SeriesFunnel
data SeriesGauge
data SeriesPictorialBar
data SeriesThemeRiver
data SeriesCustom
