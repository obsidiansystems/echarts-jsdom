{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
module ECharts.Internal where

import Control.Monad (join, void)
import Data.Aeson (ToJSON, genericToEncoding, genericToJSON, defaultOptions, Options(..))
import Data.Default (Default, def)
import qualified Data.Aeson as Aeson
import Data.Text (Text)
import qualified Data.Text as T
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Scientific
import Data.Time
import GHC.Generics (Generic)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Vector as V
import JSDOM.Types (JSVal, toJSVal, JSM, MonadJSM, liftJSM)
import Data.Proxy
import Data.Some (Some)
import qualified Data.Some as Some

import Reflex.Class (ffor)

import ECharts.Types
import ECharts.Series
import ECharts.ChartOptions

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
  -- line options
  { _eChartSeries_type :: Maybe Text
  , _eChartSeries_id :: Maybe Text
  , _eChartSeries_name :: Maybe Text
  , _eChartSeries_coordinateSystem :: Maybe CoordinateSystem
  , _eChartSeries_xAxisIndex :: Maybe Int
  , _eChartSeries_yAxisIndex :: Maybe Int
  , _eChartSeries_polarIndex :: Maybe Int
  , _eChartSeries_symbol :: Maybe Symbol
  , _eChartSeries_symbolSize :: Maybe SymbolSize
  , _eChartSeries_symbolRotate :: Maybe Int
  , _eChartSeries_symbolKeepAspect :: Maybe Bool
  , _eChartSeries_symbolOffset :: Maybe (Int, Int) -- or Text?
  , _eChartSeries_showSymbol :: Maybe Bool
  , _eChartSeries_showAllSymbol :: Maybe Bool
  , _eChartSeries_hoverAnimation :: Maybe Bool
  , _eChartSeries_legendHoverLink :: Maybe Bool
  , _eChartSeries_stack :: Maybe Text
  , _eChartSeries_cursor :: Maybe Text
  , _eChartSeries_connectNulls :: Maybe Bool
  , _eChartSeries_clipOverflow :: Maybe Bool
  , _eChartSeries_step :: Maybe Step
  , _eChartSeries_label :: Maybe Label -- common
  , _eChartSeries_itemStyle :: Maybe ItemStyle -- common style
  , _eChartSeries_lineStyle :: Maybe LineStyle
  , _eChartSeries_areaStyle :: Maybe AreaStyle
  , _eChartSeries_emphasis :: Maybe Emphasis -- common
  , _eChartSeries_smooth :: Maybe Aeson.Value
  , _eChartSeries_smoothMonotone :: Maybe SmoothMonotone
  , _eChartSeries_sampling :: Maybe Sampling
  , _eChartSeries_dimensions :: Maybe Aeson.Value
  , _eChartSeries_encode :: Maybe Encode -- common
  , _eChartSeries_seriesLayoutBy :: Maybe Text
  , _eChartSeries_datasetindex :: Maybe Text
  , _eChartSeries_data :: Maybe Aeson.Value
  , _eChartSeries_markPoint :: Maybe MarkPoint -- common
  , _eChartSeries_markLine :: Maybe MarkLine -- common
  , _eChartSeries_markArea :: Maybe MarkArea -- common
  , _eChartSeries_zlevel :: Maybe Int
  , _eChartSeries_z :: Maybe Int
  , _eChartSeries_animation :: Maybe Bool
  , _eChartSeries_animationOptions :: Maybe Animation
  , _eChartSeries_tooltip :: Maybe ToolTip -- common
  -- bar options
  -- candlestick has AbsOrPercentage
  -- , _eChartSeries_barWidth :: Maybe Int
  -- , _eChartSeries_barMaxWidth :: Maybe Int
  , _eChartSeries_barMinHeight :: Maybe Int
  , _eChartSeries_barGap :: Maybe Text
  , _eChartSeries_barCategoryGap :: Maybe Text
  , _eChartSeries_large :: Maybe Bool
  , _eChartSeries_largeThreshold :: Maybe Int
  , _eChartSeries_progressive :: Maybe Int
  , _eChartSeries_progressiveThreshold :: Maybe Int
  , _eChartSeries_progressiveChunkMode :: Maybe Text
  -- pie options
  , _eChartSeries_hoverOffset :: Maybe Int
  , _eChartSeries_selectedMode :: Maybe SelectedMode
  , _eChartSeries_selectedOffset :: Maybe Int
  , _eChartSeries_clockwise :: Maybe Bool
  , _eChartSeries_minAngle :: Maybe Int
  , _eChartSeries_roseType :: Maybe Text
  , _eChartSeries_avoidLabelOverlap :: Maybe Bool
  , _eChartSeries_stillShowZeroSum :: Maybe Bool
  , _eChartSeries_center :: Maybe (AbsOrPercentage, AbsOrPercentage)
  , _eChartSeries_radius :: Maybe (AbsOrPercentage, AbsOrPercentage)
  -- scatter options
  , _eChartSeries_geoIndex :: Maybe Int
  , _eChartSeries_calendarIndex :: Maybe Int
  -- effectScatter options
  , _eChartSeries_showEffectOn :: Maybe Text
  , _eChartSeries_rippleEffect :: Maybe RippleEffect
  -- radar options
  , _eChartSeries_radarIndex :: Maybe Int
  -- tree options
  , _eChartSeries_left :: Maybe TextOrScientific
  , _eChartSeries_top :: Maybe TextOrScientific
  , _eChartSeries_right :: Maybe TextOrScientific
  , _eChartSeries_bottom :: Maybe TextOrScientific
  , _eChartSeries_width :: Maybe TextOrScientific
  , _eChartSeries_height :: Maybe TextOrScientific
  , _eChartSeries_layout :: Maybe Text
  , _eChartSeries_orient :: Maybe Text
  , _eChartSeries_roam :: Maybe Text
  , _eChartSeries_expandAndCollapse :: Maybe Bool
  , _eChartSeries_initialTreeDepth :: Maybe Int
  , _eChartSeries_leaves :: (Maybe Label, Maybe ItemStyle, Maybe Emphasis)
  -- treemap options
  , _eChartSeries_squareRatio :: Maybe Scientific
  , _eChartSeries_leafDepth :: Maybe Int
  , _eChartSeries_drillDownIcon :: Maybe Text
  , _eChartSeries_nodeClick :: Maybe Text
  , _eChartSeries_zoomToNodeRatio :: Maybe Scientific
  , _eChartSeries_levels :: Maybe Aeson.Value
  , _eChartSeries_silent :: Maybe Aeson.Value
  , _eChartSeries_visualDimension :: Maybe Int
  , _eChartSeries_visualMin :: Maybe Int
  , _eChartSeries_visualMax :: Maybe Int
  , _eChartSeries_colorAlpha :: Maybe Aeson.Value
  , _eChartSeries_colorSaturation :: Maybe Aeson.Value
  , _eChartSeries_colorMappingsBy :: Maybe Text
  , _eChartSeries_visibleMin :: Maybe Int
  , _eChartSeries_childrenVisibleMin :: Maybe Int
  , _eChartSeries_breadcrumb :: Maybe Aeson.Value
  -- sunburst options
  , _eChartSeries_sort :: Maybe Aeson.Value
  , _eChartSeries_renderLabelForZeroData :: Maybe Bool
  , _eChartSeries_downplay :: Maybe Aeson.Value
  -- boxplot options
  , _eChartSeries_boxWidth :: Maybe (AbsOrPercentage, AbsOrPercentage)
  -- candlestick options
  , _eChartSeries_barWidth :: Maybe AbsOrPercentage
  , _eChartSeries_barMinWidth :: Maybe AbsOrPercentage
  , _eChartSeries_barMaxWidth :: Maybe AbsOrPercentage
  -- heatmap
  , _eChartSeries_blurSize :: Maybe Int
  , _eChartSeries_minOpacity :: Maybe Scientific
  , _eChartSeries_maxOpacity :: Maybe Scientific
  -- map
  , _eChartSeries_aspectScale :: Maybe Scientific
  , _eChartSeries_boundingCoords :: Maybe Aeson.Value
  , _eChartSeries_zoom :: Maybe Scientific
  , _eChartSeries_scaleLimit :: (Maybe Scientific, Maybe Scientific)
  , _eChartSeries_nameMap :: Maybe Aeson.Value
  , _eChartSeries_layoutCenter :: Maybe Aeson.Value
  , _eChartSeries_mapValueCalculation :: Maybe Text
  , _eChartSeries_showLegendSymbol :: Maybe Bool
  -- parallel
  , _eChartSeries_parallelIndex :: Maybe Int
  , _eChartSeries_inactiveOpacity :: Maybe Scientific
  , _eChartSeries_activeOpacity :: Maybe Scientific
  , _eChartSeries_realtime :: Maybe Bool
  -- lines
  , _eChartSeries_polyline :: Maybe Bool
  -- graph
  , _eChartSeries_circularRotateLayout :: Maybe Bool
  , _eChartSeries_force :: Maybe Aeson.Value
  , _eChartSeries_nodeScaleRatio :: Maybe Scientific
  , _eChartSeries_draggable :: Maybe Bool
  -- focusNodeAdjacency :: Maybe Bool -- sankey has more options
  , _eChartSeries_edgeSymbol :: Maybe Aeson.Value
  , _eChartSeries_edgeSymbolSize :: Maybe Aeson.Value
  , _eChartSeries_categories :: Maybe Aeson.Value
  , _eChartSeries_links :: Maybe Aeson.Value
  -- sankey
  , _eChartSeries_nodeWidth :: Maybe Scientific
  , _eChartSeries_nodeGap :: Maybe Scientific
  , _eChartSeries_layoutIterations :: Maybe Int
  , _eChartSeries_focusNodeAdjacency :: Maybe (Either Bool Text)
  -- funnel
  , _eChartSeries_min :: Maybe Scientific
  , _eChartSeries_max :: Maybe Scientific
  , _eChartSeries_minSize :: Maybe AbsOrPercentage
  , _eChartSeries_maxSize :: Maybe AbsOrPercentage
  , _eChartSeries_gap :: Maybe Scientific
  , _eChartSeries_funnelAlign :: Maybe Text
  , _eChartSeries_labelLine :: Maybe Aeson.Value
  -- gauge
  , _eChartSeries_startAngle :: Maybe Scientific
  , _eChartSeries_endAngle :: Maybe Scientific
  , _eChartSeries_splitNumber :: Maybe Scientific
  , _eChartSeries_axisLine :: Maybe Aeson.Value
  , _eChartSeries_splitLine :: Maybe Aeson.Value
  , _eChartSeries_axisTick :: Maybe Aeson.Value
  , _eChartSeries_axisLabel :: Maybe Aeson.Value
  , _eChartSeries_pointer :: Maybe Aeson.Value
  , _eChartSeries_details :: Maybe Aeson.Value
  -- pictorialBar
  -- symbol stuff
  -- themeRiver
  -- custom
  -- other common options
  }
  deriving (Generic)

instance Default EChartSeries where

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
              ]) d
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
    -- toEChartSeries x = def
      -- Series_Line n d smooth stack ->
      --   let d' = case d of
      --         Nothing -> Nothing
      --         Just xs -> Just $ Aeson.Array $ V.fromList $ fmap Aeson.toJSON xs
      --   in def
      --         { _eChartSeries_type = Just "line"
      --         , _eChartSeries_name = n
      --         , _eChartSeries_data = d'
      --         , _eChartSeries_smooth = smooth
      --         , _eChartSeries_stack = stack
      --         }
      -- Series_Timeline n timeX d smooth stack areaStyle ->
      --   let d' = case d of
      --         Nothing -> Nothing
      --         Just xs -> Just $ Aeson.Array $ V.fromList $
      --           -- Note: If more than three digits of precision is provided for
      --           -- the seconds, echarts interprets that as a larger number of
      --           -- milliseconds, not more precision.  E.g.: "0.123456" seconds
      --           -- is interpreted as 123.456 seconds
      --           ffor xs $ \(t, v) -> Aeson.Object $ HashMap.fromList
      --             [ ("name", Aeson.String $ T.pack $ show t)
      --             , ("value", (if timeX then Aeson.toJSON else Aeson.toJSON . swap) (t, v))
      --             ]
      --   in def
      --         { _eChartSeries_type = Just "line"
      --         , _eChartSeries_name = n
      --         , _eChartSeries_data = d'
      --         , _eChartSeries_smooth = smooth
      --         , _eChartSeries_animation = Nothing
      --         , _eChartSeries_stack = stack
      --         , _eChartSeries_areaStyle = "auto" <$ areaStyle -- TODO
      --         , _eChartSeries_symbol = Just "none"
      --         }
    swap (x, y) = (y, x)

data ECharts = ECharts { unECharts :: JSVal }

toEChartSeries :: Some SeriesT -> EChartSeries
toEChartSeries (Some.This st) = def
  -- common options
  { _eChartSeries_type = Just $ getSeriesType st
  , _eChartSeries_id = _series_id s
  , _eChartSeries_name = _series_name s
  -- SeriesOptions
  , _eChartSeries_data                   = series_data_toJson st
  -- , _eChartSeries_coordinateSystem       = _series_coordinateSystem       s
  -- , _eChartSeries_xAxisIndex             = _series_xAxisIndex             s
  -- , _eChartSeries_yAxisIndex             = _series_yAxisIndex             s
  -- , _eChartSeries_polarIndex             = _series_polarIndex             s
  -- , _eChartSeries_symbol                 = _series_symbol                 s
  -- , _eChartSeries_symbolSize             = _series_symbolSize             s
  -- , _eChartSeries_symbolRotate           = _series_symbolRotate           s
  -- , _eChartSeries_symbolKeepAspect       = _series_symbolKeepAspect       s
  -- , _eChartSeries_symbolOffset           = _series_symbolOffset           s
  -- , _eChartSeries_showSymbol             = _series_showSymbol             s
  -- , _eChartSeries_showAllSymbol          = _series_showAllSymbol          s
  -- , _eChartSeries_hoverAnimation         = _series_hoverAnimation         s
  -- , _eChartSeries_legendHoverLink        = _series_legendHoverLink        s
  -- , _eChartSeries_stack                  = _series_stack                  s
  -- , _eChartSeries_cursor                 = _series_cursor                 s
  -- , _eChartSeries_connectNulls           = _series_connectNulls           s
  -- , _eChartSeries_clipOverflow           = _series_clipOverflow           s
  -- , _eChartSeries_step                   = _series_step                   s
  -- , _eChartSeries_label                  = _series_label                  s
  -- , _eChartSeries_itemStyle              = _series_itemStyle              s
  -- , _eChartSeries_lineStyle              = _series_lineStyle              s
  -- , _eChartSeries_areaStyle              = _series_areaStyle              s
  -- , _eChartSeries_emphasis               = _series_emphasis               s
  , _eChartSeries_smooth                 = series_smooth_toJson st
  -- , _eChartSeries_smoothMonotone         = _series_smoothMonotone         s
  -- , _eChartSeries_sampling               = _series_sampling               s
  -- , _eChartSeries_dimensions             = _series_dimensions             s
  -- , _eChartSeries_encode                 = _series_encode                 s
  -- , _eChartSeries_seriesLayoutBy         = _series_seriesLayoutBy         s
  -- , _eChartSeries_datasetindex           = _series_datasetindex           s
  -- , _eChartSeries_markPoint              = _series_markPoint              s
  -- , _eChartSeries_markLine               = _series_markLine               s
  -- , _eChartSeries_markArea               = _series_markArea               s
  -- , _eChartSeries_zlevel                 = _series_zlevel                 s
  -- , _eChartSeries_z                      = _series_z                      s
  -- , _eChartSeries_animation              = _series_animation              s
  -- , _eChartSeries_animationOptions       = _series_animationOptions       s
  -- , _eChartSeries_tooltip                = _series_tooltip                s
  -- , _eChartSeries_barMinHeight           = _series_barMinHeight           s
  -- , _eChartSeries_barGap                 = _series_barGap                 s
  -- , _eChartSeries_barCategoryGap         = _series_barCategoryGap         s
  -- , _eChartSeries_large                  = _series_large                  s
  -- , _eChartSeries_largeThreshold         = _series_largeThreshold         s
  -- , _eChartSeries_progressive            = _series_progressive            s
  -- , _eChartSeries_progressiveThreshold   = _series_progressiveThreshold   s
  -- , _eChartSeries_progressiveChunkMode   = _series_progressiveChunkMode   s
  -- , _eChartSeries_hoverOffset            = _series_hoverOffset            s
  -- , _eChartSeries_selectedMode           = _series_selectedMode           s
  -- , _eChartSeries_selectedOffset         = _series_selectedOffset         s
  -- , _eChartSeries_clockwise              = _series_clockwise              s
  -- , _eChartSeries_minAngle               = _series_minAngle               s
  -- , _eChartSeries_roseType               = _series_roseType               s
  -- , _eChartSeries_avoidLabelOverlap      = _series_avoidLabelOverlap      s
  -- , _eChartSeries_stillShowZeroSum       = _series_stillShowZeroSum       s
  -- , _eChartSeries_center                 = _series_center                 s
  -- , _eChartSeries_radius                 = _series_radius                 s
  -- , _eChartSeries_geoIndex               = _series_geoIndex               s
  -- , _eChartSeries_calendarIndex          = _series_calendarIndex          s
  -- , _eChartSeries_showEffectOn           = _series_showEffectOn           s
  -- , _eChartSeries_rippleEffect           = _series_rippleEffect           s
  -- , _eChartSeries_radarIndex             = _series_radarIndex             s
  -- , _eChartSeries_left                   = _series_left                   s
  -- , _eChartSeries_top                    = _series_top                    s
  -- , _eChartSeries_right                  = _series_right                  s
  -- , _eChartSeries_bottom                 = _series_bottom                 s
  -- , _eChartSeries_width                  = _series_width                  s
  -- , _eChartSeries_height                 = _series_height                 s
  -- , _eChartSeries_layout                 = _series_layout                 s
  -- , _eChartSeries_orient                 = _series_orient                 s
  -- , _eChartSeries_roam                   = _series_roam                   s
  -- , _eChartSeries_expandAndCollapse      = _series_expandAndCollapse      s
  -- , _eChartSeries_initialTreeDepth       = _series_initialTreeDepth       s
  -- , _eChartSeries_leaves                 = _series_leaves                 s
  -- , _eChartSeries_squareRatio            = _series_squareRatio            s
  -- , _eChartSeries_leafDepth              = _series_leafDepth              s
  -- , _eChartSeries_drillDownIcon          = _series_drillDownIcon          s
  -- , _eChartSeries_nodeClick              = _series_nodeClick              s
  -- , _eChartSeries_zoomToNodeRatio        = _series_zoomToNodeRatio        s
  -- , _eChartSeries_levels                 = _series_levels                 s
  -- , _eChartSeries_silent                 = _series_silent                 s
  -- , _eChartSeries_visualDimension        = _series_visualDimension        s
  -- , _eChartSeries_visualMin              = _series_visualMin              s
  -- , _eChartSeries_visualMax              = _series_visualMax              s
  -- , _eChartSeries_colorAlpha             = _series_colorAlpha             s
  -- , _eChartSeries_colorSaturation        = _series_colorSaturation        s
  -- , _eChartSeries_colorMappingsBy        = _series_colorMappingsBy        s
  -- , _eChartSeries_visibleMin             = _series_visibleMin             s
  -- , _eChartSeries_childrenVisibleMin     = _series_childrenVisibleMin     s
  -- , _eChartSeries_breadcrumb             = _series_breadcrumb             s
  -- , _eChartSeries_sort                   = _series_sort                   s
  -- , _eChartSeries_renderLabelForZeroData = _series_renderLabelForZeroData s
  -- , _eChartSeries_downplay               = _series_downplay               s
  -- , _eChartSeries_boxWidth               = _series_boxWidth               s
  -- , _eChartSeries_barWidth               = _series_barWidth               s
  -- , _eChartSeries_barMinWidth            = _series_barMinWidth            s
  -- , _eChartSeries_barMaxWidth            = _series_barMaxWidth            s
  -- , _eChartSeries_blurSize               = _series_blurSize               s
  -- , _eChartSeries_minOpacity             = _series_minOpacity             s
  -- , _eChartSeries_maxOpacity             = _series_maxOpacity             s
  -- , _eChartSeries_aspectScale            = _series_aspectScale            s
  -- , _eChartSeries_boundingCoords         = _series_boundingCoords         s
  -- , _eChartSeries_zoom                   = _series_zoom                   s
  -- , _eChartSeries_scaleLimit             = _series_scaleLimit             s
  -- , _eChartSeries_nameMap                = _series_nameMap                s
  -- , _eChartSeries_layoutCenter           = _series_layoutCenter           s
  -- , _eChartSeries_mapValueCalculation    = _series_mapValueCalculation    s
  -- , _eChartSeries_showLegendSymbol       = _series_showLegendSymbol       s
  -- , _eChartSeries_parallelIndex          = _series_parallelIndex          s
  -- , _eChartSeries_inactiveOpacity        = _series_inactiveOpacity        s
  -- , _eChartSeries_activeOpacity          = _series_activeOpacity          s
  -- , _eChartSeries_realtime               = _series_realtime               s
  -- , _eChartSeries_polyline               = _series_polyline               s
  -- , _eChartSeries_circularRotateLayout   = _series_circularRotateLayout   s
  -- , _eChartSeries_force                  = _series_force                  s
  -- , _eChartSeries_nodeScaleRatio         = _series_nodeScaleRatio         s
  -- , _eChartSeries_draggable              = _series_draggable              s
  -- , _eChartSeries_edgeSymbol             = _series_edgeSymbol             s
  -- , _eChartSeries_edgeSymbolSize         = _series_edgeSymbolSize         s
  -- , _eChartSeries_categories             = _series_categories             s
  -- , _eChartSeries_links                  = _series_links                  s
  -- , _eChartSeries_nodeWidth              = _series_nodeWidth              s
  -- , _eChartSeries_nodeGap                = _series_nodeGap                s
  -- , _eChartSeries_layoutIterations       = _series_layoutIterations       s
  -- , _eChartSeries_focusNodeAdjacency     = _series_focusNodeAdjacency     s
  -- , _eChartSeries_min                    = _series_min                    s
  -- , _eChartSeries_max                    = _series_max                    s
  -- , _eChartSeries_minSize                = _series_minSize                s
  -- , _eChartSeries_maxSize                = _series_maxSize                s
  -- , _eChartSeries_gap                    = _series_gap                    s
  -- , _eChartSeries_funnelAlign            = _series_funnelAlign            s
  -- , _eChartSeries_labelLine              = _series_labelLine              s
  -- , _eChartSeries_startAngle             = _series_startAngle             s
  -- , _eChartSeries_endAngle               = _series_endAngle               s
  -- , _eChartSeries_splitNumber            = _series_splitNumber            s
  -- , _eChartSeries_axisLine               = _series_axisLine               s
  -- , _eChartSeries_splitLine              = _series_splitLine              s
  -- , _eChartSeries_axisTick               = _series_axisTick               s
  -- , _eChartSeries_axisLabel              = _series_axisLabel              s
  -- , _eChartSeries_pointer                = _series_pointer                s
  -- , _eChartSeries_details                = _series_details                s
  }
  where
    s = getSeries st
