{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module ECharts.Internal.EChartTypes where

import Data.Aeson (ToJSON, genericToEncoding, genericToJSON, defaultOptions, Options(..))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Scientific
import Data.Time
import GHC.Generics (Generic)
import Data.Default (Default, def)
import qualified Data.Aeson as Aeson

import ECharts.DeriveToJSVal (toJSVal_generic, ToJSVal(..))
import ECharts.Types
import ECharts.Internal.EChartLabel

type EChartIconStyle = Aeson.Value

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

instance ToJSVal EChartTextStyle where
  toJSVal = toJSVal_generic (drop $ T.length "_eChartTextStyle_")

instance ToJSON EChartTextStyle where
  toJSON = genericToJSON $ defaultOptions
    { fieldLabelModifier = drop $ T.length "_eChartTextStyle_"
    , omitNothingFields = True
    }
  toEncoding = genericToEncoding $ defaultOptions
    { fieldLabelModifier = drop $ T.length "_eChartTextStyle_"
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
  , _eChartAxis_splitLine :: Maybe SplitLine
  , _eChartAxis_splitArea :: Maybe SplitArea
  , _eChartAxis_splitNumber :: Maybe Int
  , _eChartAxis_axisLine :: Maybe EChartAxisLine
  , _eChartAxis_axisTick :: Maybe EChartAxisTick
  , _eChartAxis_axisLabel :: Maybe EChartAxisLabel
  , _eChartAxis_data :: Maybe Aeson.Value
  , _eChartAxis_zlevel :: Maybe Int
  , _eChartAxis_z :: Maybe Int
  }
  deriving (Generic)

instance ToJSVal EChartAxis where
  toJSVal = toJSVal_generic (drop $ T.length "_eChartAxis_")

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

instance ToJSVal EChartAxisLine where
  toJSVal = toJSVal_generic (drop $ T.length "_eChartAxisLine_")

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

instance ToJSVal EChartLineStyle where
  toJSVal = toJSVal_generic (drop $ T.length "_eChartLineStyle_")

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

instance ToJSVal EChartAxisTick where
  toJSVal = toJSVal_generic (drop $ T.length "_eChartAxisTick_")

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

instance ToJSVal EChartAxisLabel where
  toJSVal = toJSVal_generic (drop $ T.length "_eChartAxisLabel_")

instance ToJSON EChartAxisLabel where
  toJSON = genericToJSON $ defaultOptions
    { fieldLabelModifier = drop $ T.length "_eChartAxisLabel_"
    , omitNothingFields = True
    }
  toEncoding = genericToEncoding $ defaultOptions
    { fieldLabelModifier = drop $ T.length "_eChartAxisLabel_"
    , omitNothingFields = True
    }

toEChartTextStyle :: TextStyle -> EChartTextStyle
toEChartTextStyle v = EChartTextStyle
  { _eChartTextStyle_color = _textStyle_color v
  , _eChartTextStyle_fontStyle = _font_style =<< _textStyle_font v
  , _eChartTextStyle_fontWeight = _font_weight =<< _textStyle_font v
  , _eChartTextStyle_fontFamily = _font_family =<< _textStyle_font v
  , _eChartTextStyle_fontSize = _font_size =<< _textStyle_font v
  , _eChartTextStyle_align = _textStyle_align v
  , _eChartTextStyle_verticalAlign = _textStyle_verticalAlign v
  , _eChartTextStyle_lineHeight = _textStyle_lineHeight v
  , _eChartTextStyle_width = sizeValueToSN <$> (_textStyle_width v)
  , _eChartTextStyle_height = sizeValueToSN <$> (_textStyle_height v)
  , _eChartTextStyle_textBorderColor = _border_color =<< _textStyle_textBorder v
  , _eChartTextStyle_textBorderWidth = _border_width =<< _textStyle_textBorder v
  , _eChartTextStyle_textShadowColor = _shadow_color =<< _textStyle_textShadow v
  , _eChartTextStyle_textShadowBlur = _shadow_blur =<< _textStyle_textShadow v
  , _eChartTextStyle_textShadowOffsetX = _shadow_offsetX =<< _textStyle_textShadow v
  , _eChartTextStyle_textShadowOffsetY = _shadow_offsetY =<< _textStyle_textShadow v
  }

instance ToJSVal TextStyle where
  toJSVal = toJSVal . toEChartTextStyle

instance ToJSON TextStyle where
  toJSON = Aeson.toJSON . toEChartTextStyle

    -- toEChartTextStyle :: TextStyle -> EChartTextStyle
    -- toEChartTextStyle t = EChartTextStyle
    --   { _eChartTextStyle_color = _textStyle_color t
    --   , _eChartTextStyle_fontStyle = join $ fmap _font_style $ _textStyle_font t
    --   , _eChartTextStyle_fontWeight = join $ fmap _font_weight $ _textStyle_font t
    --   , _eChartTextStyle_fontFamily = join $ fmap _font_family $ _textStyle_font t
    --   , _eChartTextStyle_fontSize = join $ fmap _font_size $ _textStyle_font t
    --   , _eChartTextStyle_align = _textStyle_align t
    --   , _eChartTextStyle_verticalAlign = _textStyle_verticalAlign t
    --   , _eChartTextStyle_lineHeight = _textStyle_lineHeight t
    --   , _eChartTextStyle_width = fmap sizeValueToSN $ _textStyle_width t
    --   , _eChartTextStyle_height = fmap sizeValueToSN $ _textStyle_height t
    --   , _eChartTextStyle_textBorderColor = join $ fmap _border_color $ _textStyle_textBorder t
    --   , _eChartTextStyle_textBorderWidth = join $ fmap _border_width $ _textStyle_textBorder t
    --   , _eChartTextStyle_textShadowColor = join $ fmap _shadow_color $ _textStyle_textShadow t
    --   , _eChartTextStyle_textShadowBlur = join $ fmap _shadow_blur $ _textStyle_textShadow t
    --   , _eChartTextStyle_textShadowOffsetX = join $ fmap _shadow_offsetX $ _textStyle_textShadow t
    --   , _eChartTextStyle_textShadowOffsetY = join $ fmap _shadow_offsetY $ _textStyle_textShadow t
    --   }

data EChartDataZoom = EChartDataZoom
  { _eChartDataZoom_show :: Maybe Bool
  , _eChartDataZoom_id :: Maybe Text
  , _eChartDataZoom_type :: Maybe Text
  , _eChartDataZoom_disabled :: Maybe Bool
  , _eChartDataZoom_xAxisIndex :: Maybe [Int]
  , _eChartDataZoom_yAxisIndex :: Maybe [Int]
  , _eChartDataZoom_radiusAxisIndex :: Maybe [Int]
  , _eChartDataZoom_angleAxisIndex :: Maybe [Int]
  , _eChartDataZoom_filterMode :: Maybe Text
  , _eChartDataZoom_start :: Maybe Aeson.Value
  , _eChartDataZoom_end :: Maybe Aeson.Value
  , _eChartDataZoom_minSpan :: Maybe Int
  , _eChartDataZoom_maxSpan :: Maybe Int
  , _eChartDataZoom_minValueSpan :: Maybe Aeson.Value
  , _eChartDataZoom_maxValueSpan :: Maybe Aeson.Value
  , _eChartDataZoom_orient :: Maybe Text
  , _eChartDataZoom_zoomLock :: Maybe Bool
  , _eChartDataZoom_throttle :: Maybe Int
  , _eChartDataZoom_rangeMode :: Maybe [Text]
  , _eChartDataZoom_zoomOnMouseWheel :: Maybe Bool
  , _eChartDataZoom_moveOnMouseMove :: Maybe Bool
  , _eChartDataZoom_moveOnMouseWheel :: Maybe Bool
  , _eChartDataZoom_preventDefaultMouseMove :: Maybe Bool
  , _eChartDataZoom_backgroundColor :: Maybe Text
  , _eChartDataZoom_dataBackground :: Maybe Aeson.Value
  , _eChartDataZoom_fillerColor :: Maybe Text
  , _eChartDataZoom_borderColor :: Maybe Text
  , _eChartDataZoom_handleIcon :: Maybe Text
  , _eChartDataZoom_handleSize :: Maybe SN
  , _eChartDataZoom_handleStyle :: Maybe EChartItemStyle
  , _eChartDataZoom_labelPrecision :: Maybe Int
  , _eChartDataZoom_labelFormatter :: Maybe Aeson.Value
  , _eChartDataZoom_showDetail :: Maybe Bool
  , _eChartDataZoom_showDataShadow :: Maybe Text
  , _eChartDataZoom_realtime :: Maybe Bool
  , _eChartDataZoom_textStyle :: Maybe EChartTextStyle
  , _eChartDataZoom_startValue :: Maybe Aeson.Value
  , _eChartDataZoom_endValue :: Maybe Aeson.Value
  , _eChartDataZoom_zlevel :: Maybe Int
  , _eChartDataZoom_z :: Maybe Int
  , _eChartDataZoom_left :: Maybe SN
  , _eChartDataZoom_right :: Maybe SN
  , _eChartDataZoom_top :: Maybe SN
  , _eChartDataZoom_bottom :: Maybe SN
  }
  deriving (Generic)

toEChartDataZoom :: DataZoom -> EChartDataZoom
toEChartDataZoom v = EChartDataZoom
  { _eChartDataZoom_show = _dataZoom_show v
  , _eChartDataZoom_id = _dataZoom_id v
  , _eChartDataZoom_type = _dataZoom_type v
  , _eChartDataZoom_disabled = _dataZoom_disabled v
  , _eChartDataZoom_xAxisIndex = _dataZoom_xAxisIndex v
  , _eChartDataZoom_yAxisIndex = _dataZoom_yAxisIndex v
  , _eChartDataZoom_radiusAxisIndex = _dataZoom_radiusAxisIndex v
  , _eChartDataZoom_angleAxisIndex = _dataZoom_angleAxisIndex v
  , _eChartDataZoom_filterMode = _dataZoom_filterMode v
  , _eChartDataZoom_start = _dataZoom_start v
  , _eChartDataZoom_end = _dataZoom_end v
  , _eChartDataZoom_minSpan = _dataZoom_minSpan v
  , _eChartDataZoom_maxSpan = _dataZoom_maxSpan v
  , _eChartDataZoom_minValueSpan = _dataZoom_minValueSpan v
  , _eChartDataZoom_maxValueSpan = _dataZoom_maxValueSpan v
  , _eChartDataZoom_orient = _dataZoom_orient v
  , _eChartDataZoom_zoomLock = _dataZoom_zoomLock v
  , _eChartDataZoom_throttle = _dataZoom_throttle v
  , _eChartDataZoom_rangeMode = _dataZoom_rangeMode v
  , _eChartDataZoom_zoomOnMouseWheel = _dataZoom_zoomOnMouseWheel v
  , _eChartDataZoom_moveOnMouseMove = _dataZoom_moveOnMouseMove v
  , _eChartDataZoom_moveOnMouseWheel = _dataZoom_moveOnMouseWheel v
  , _eChartDataZoom_preventDefaultMouseMove = _dataZoom_preventDefaultMouseMove v
  , _eChartDataZoom_backgroundColor = _dataZoom_backgroundColor v
  , _eChartDataZoom_dataBackground = _dataZoom_dataBackground v
  , _eChartDataZoom_fillerColor = _dataZoom_fillerColor v
  , _eChartDataZoom_borderColor = _dataZoom_borderColor v
  , _eChartDataZoom_handleIcon = _dataZoom_handleIcon v
  , _eChartDataZoom_handleSize = _dataZoom_handleSize v
  , _eChartDataZoom_handleStyle = fmap toEChartItemStyle $ _dataZoom_handleStyle v
  , _eChartDataZoom_labelPrecision = _dataZoom_labelPrecision v
  , _eChartDataZoom_labelFormatter = _dataZoom_labelFormatter v
  , _eChartDataZoom_showDetail = _dataZoom_showDetail v
  , _eChartDataZoom_showDataShadow = _dataZoom_showDataShadow v
  , _eChartDataZoom_realtime = _dataZoom_realtime v
  , _eChartDataZoom_textStyle = toEChartTextStyle <$> _dataZoom_textStyle v
  , _eChartDataZoom_startValue = _dataZoom_startValue v
  , _eChartDataZoom_endValue = _dataZoom_endValue v
  , _eChartDataZoom_zlevel = _pos_zlevel =<< _dataZoom_pos v
  , _eChartDataZoom_z = _pos_z =<< _dataZoom_pos v
  , _eChartDataZoom_left = posAlignToSN <$> (_pos_left =<< _dataZoom_pos v)
  , _eChartDataZoom_right = posAlignToSN <$> (_pos_right =<< _dataZoom_pos v)
  , _eChartDataZoom_top = posAlignToSN <$> (_pos_top =<< _dataZoom_pos v)
  , _eChartDataZoom_bottom = posAlignToSN <$> (_pos_bottom =<< _dataZoom_pos v)
  }

instance ToJSVal EChartDataZoom where
  toJSVal = toJSVal_generic (drop $ T.length "_eChartDataZoom_")

instance ToJSON EChartDataZoom where
  toJSON = genericToJSON (defaultOptions
    { fieldLabelModifier = drop (T.length "_eChartDataZoom_")
    , omitNothingFields = True
    })
  toEncoding = genericToEncoding (defaultOptions
    { fieldLabelModifier = drop (T.length "_eChartDataZoom_")
    , omitNothingFields = True
    })

data EChartItemStyle = EChartItemStyle
  { _eChartItemStyle_color :: Maybe Text
  , _eChartItemStyle_borderColor :: Maybe Text
  , _eChartItemStyle_borderWidth :: Maybe Int
  , _eChartItemStyle_borderType :: Maybe Text
  , _eChartItemStyle_shadowColor :: Maybe Text
  , _eChartItemStyle_shadowBlur :: Maybe Int
  , _eChartItemStyle_shadowOffsetX :: Maybe Int
  , _eChartItemStyle_shadowOffsetY :: Maybe Int
  , _eChartItemStyle_opacity :: Maybe Scientific
  }
  deriving (Generic)

instance ToJSVal EChartItemStyle where
  toJSVal = toJSVal_generic (drop $ T.length "_eChartItemStyle_")

instance ToJSON EChartItemStyle where
  toJSON = genericToJSON (defaultOptions
    { fieldLabelModifier = drop (T.length "_eChartItemStyle_")
    , omitNothingFields = True
    })
  toEncoding = genericToEncoding (defaultOptions
    { fieldLabelModifier = drop (T.length "_eChartItemStyle_")
    , omitNothingFields = True
    })

toEChartItemStyle :: ItemStyle -> EChartItemStyle
toEChartItemStyle v = EChartItemStyle
  { _eChartItemStyle_color = _itemStyle_color v
  , _eChartItemStyle_borderColor = _border_color =<< _itemStyle_border v
  , _eChartItemStyle_borderWidth = _border_width =<< _itemStyle_border v
  , _eChartItemStyle_borderType = _border_type =<< _itemStyle_border v
  , _eChartItemStyle_shadowColor = _shadow_color =<< _itemStyle_shadow v
  , _eChartItemStyle_shadowBlur = _shadow_blur =<< _itemStyle_shadow v
  , _eChartItemStyle_shadowOffsetX = _shadow_offsetX =<< _itemStyle_shadow v
  , _eChartItemStyle_shadowOffsetY = _shadow_offsetY =<< _itemStyle_shadow v
  , _eChartItemStyle_opacity = _itemStyle_opacity v
  }

data EChartVisualMap = EChartVisualMap
  { _eChartVisualMap_show :: Maybe Bool
  , _eChartVisualMap_id :: Maybe Text
  , _eChartVisualMap_type :: Maybe Text
  , _eChartVisualMap_splitNumber :: Maybe Int
  , _eChartVisualMap_pieces :: Maybe Aeson.Value
  , _eChartVisualMap_categories :: Maybe [Text]
  , _eChartVisualMap_min :: Maybe Int
  , _eChartVisualMap_max :: Maybe Int
  , _eChartVisualMap_minOpen :: Maybe Bool
  , _eChartVisualMap_maxOpen :: Maybe Bool
  , _eChartVisualMap_selectedMode :: Maybe Text
  , _eChartVisualMap_inverse :: Maybe Bool
  , _eChartVisualMap_precision :: Maybe Int
  , _eChartVisualMap_itemWidth :: Maybe Int
  , _eChartVisualMap_itemHeight :: Maybe Int
  , _eChartVisualMap_align :: Maybe Text
  , _eChartVisualMap_text :: Maybe [Text]
  , _eChartVisualMap_textGap :: Maybe [Int]
  , _eChartVisualMap_showLabel :: Maybe Bool
  , _eChartVisualMap_itemGap :: Maybe Int
  , _eChartVisualMap_itemSymbol :: Maybe Text
  , _eChartVisualMap_dimension :: Maybe Text
  , _eChartVisualMap_seriesIndex :: Maybe SN
  , _eChartVisualMap_hoverLink :: Maybe Bool
  , _eChartVisualMap_inRange :: Maybe Aeson.Value
  , _eChartVisualMap_outOfRange :: Maybe Aeson.Value
  , _eChartVisualMap_controller :: Maybe Aeson.Value
  , _eChartVisualMap_zlevel :: Maybe Int
  , _eChartVisualMap_z :: Maybe Int
  , _eChartVisualMap_left :: Maybe SN
  , _eChartVisualMap_right :: Maybe SN
  , _eChartVisualMap_top :: Maybe SN
  , _eChartVisualMap_bottom :: Maybe SN
  , _eChartVisualMap_padding :: Maybe Int
  , _eChartVisualMap_orient :: Maybe Text
  , _eChartVisualMap_backgroundColor :: Maybe Text
  , _eChartVisualMap_borderColor :: Maybe Text
  , _eChartVisualMap_borderWidth :: Maybe Int
  , _eChartVisualMap_textStyle :: Maybe EChartTextStyle
  , _eChartVisualMap_formatter :: Maybe Aeson.Value
  }
  deriving (Generic)

toEChartVisualMap :: VisualMap -> EChartVisualMap
toEChartVisualMap v = EChartVisualMap
  { _eChartVisualMap_show = _visualMap_show v
  , _eChartVisualMap_id = _visualMap_id v
  , _eChartVisualMap_type = _visualMap_type v
  , _eChartVisualMap_splitNumber = _visualMap_splitNumber v
  , _eChartVisualMap_pieces = _visualMap_pieces v
  , _eChartVisualMap_categories = _visualMap_categories v
  , _eChartVisualMap_min = _visualMap_min v
  , _eChartVisualMap_max = _visualMap_max v
  , _eChartVisualMap_minOpen = _visualMap_minOpen v
  , _eChartVisualMap_maxOpen = _visualMap_maxOpen v
  , _eChartVisualMap_selectedMode = _visualMap_selectedMode v
  , _eChartVisualMap_inverse = _visualMap_inverse v
  , _eChartVisualMap_precision = _visualMap_precision v
  , _eChartVisualMap_itemWidth = _visualMap_itemWidth v
  , _eChartVisualMap_itemHeight = _visualMap_itemHeight v
  , _eChartVisualMap_align = _visualMap_align v
  , _eChartVisualMap_text = _visualMap_text v
  , _eChartVisualMap_textGap = _visualMap_textGap v
  , _eChartVisualMap_showLabel = _visualMap_showLabel v
  , _eChartVisualMap_itemGap = _visualMap_itemGap v
  , _eChartVisualMap_itemSymbol = _visualMap_itemSymbol v
  , _eChartVisualMap_dimension = _visualMap_dimension v
  , _eChartVisualMap_seriesIndex = _visualMap_seriesIndex v
  , _eChartVisualMap_hoverLink = _visualMap_hoverLink v
  , _eChartVisualMap_inRange = Aeson.toJSON <$> _visualMap_inRange v
  , _eChartVisualMap_outOfRange = Aeson.toJSON <$> _visualMap_outOfRange v

  , _eChartVisualMap_controller = Nothing
  , _eChartVisualMap_zlevel = _pos_zlevel =<< _visualMap_pos v
  , _eChartVisualMap_z = _pos_z =<< _visualMap_pos v
  , _eChartVisualMap_left = posAlignToSN <$> (_pos_left =<< _visualMap_pos v)
  , _eChartVisualMap_right = posAlignToSN <$> (_pos_right =<< _visualMap_pos v)
  , _eChartVisualMap_top = posAlignToSN <$> (_pos_top =<< _visualMap_pos v)
  , _eChartVisualMap_bottom = posAlignToSN <$> (_pos_bottom =<< _visualMap_pos v)
  , _eChartVisualMap_padding = _visualMap_padding v
  , _eChartVisualMap_orient = _visualMap_orient v
  , _eChartVisualMap_backgroundColor = _visualMap_backgroundColor v
  , _eChartVisualMap_borderWidth = _border_width =<< _visualMap_border v
  , _eChartVisualMap_borderColor = _border_color =<< _visualMap_border v
  , _eChartVisualMap_textStyle = toEChartTextStyle <$> _visualMap_textStyle v
  , _eChartVisualMap_formatter = _visualMap_formatter v
  }

instance ToJSVal EChartVisualMap where
  toJSVal = toJSVal_generic (drop $ T.length "_eChartVisualMap_")

instance ToJSON EChartVisualMap where
  toJSON = genericToJSON (defaultOptions
    { fieldLabelModifier = drop (T.length "_eChartVisualMap_")
    , omitNothingFields = True
    })
  toEncoding = genericToEncoding (defaultOptions
    { fieldLabelModifier = drop (T.length "_eChartVisualMap_")
    , omitNothingFields = True
    })


-- Instances for Types
instance ToJSVal MarkArea where
  toJSVal = toJSVal_generic (drop $ T.length "_markArea_")

instance ToJSON MarkArea where
  toJSON = genericToJSON $ defaultOptions
    { fieldLabelModifier = drop $ T.length "_markArea_"
    , omitNothingFields = True
    }
  toEncoding = genericToEncoding $ defaultOptions
    { fieldLabelModifier = drop $ T.length "_markArea_"
    , omitNothingFields = True
    }

instance ToJSVal MarkLine where
  toJSVal = toJSVal_generic (drop $ T.length "_markLine_")

instance ToJSON MarkLine where
  toJSON = genericToJSON $ defaultOptions
    { fieldLabelModifier = drop $ T.length "_markLine_"
    , omitNothingFields = True
    }
  toEncoding = genericToEncoding $ defaultOptions
    { fieldLabelModifier = drop $ T.length "_markLine_"
    , omitNothingFields = True
    }

instance ToJSVal ItemStyle where
  toJSVal = toJSVal . toEChartItemStyle

instance ToJSON ItemStyle where
  toJSON = Aeson.toJSON . toEChartItemStyle

instance ToJSVal EChartLabel where
  toJSVal = toJSVal_generic (drop $ T.length "_eChartLabel_")

instance ToJSON EChartLabel where
  toJSON = genericToJSON $ defaultOptions
    { fieldLabelModifier = drop $ T.length "_eChartLabel_"
    , omitNothingFields = True
    }
  toEncoding = genericToEncoding $ defaultOptions
    { fieldLabelModifier = drop $ T.length "_eChartLabel_"
    , omitNothingFields = True
    }

instance ToJSVal Label where
  toJSVal = toJSVal . toEChartLabel

instance ToJSON Label where
  toJSON = Aeson.toJSON . toEChartLabel

instance ToJSVal AxisPointer where
  toJSVal = toJSVal_generic (drop $ T.length "_axisPointer_")

instance ToJSON AxisPointer where
  toJSON = genericToJSON $ defaultOptions
    { fieldLabelModifier = drop $ T.length "_axisPointer_"
    , omitNothingFields = True
    }
  toEncoding = genericToEncoding $ defaultOptions
    { fieldLabelModifier = drop $ T.length "_axisPointer_"
    , omitNothingFields = True
    }

instance ToJSVal SplitLine where
  toJSVal = toJSVal_generic (drop $ T.length "_splitLine_")

instance ToJSON SplitLine where
  toJSON = genericToJSON $ defaultOptions
    { fieldLabelModifier = drop $ T.length "_splitLine_"
    , omitNothingFields = True
    }
  toEncoding = genericToEncoding $ defaultOptions
    { fieldLabelModifier = drop $ T.length "_splitLine_"
    , omitNothingFields = True
    }

instance ToJSVal SplitArea where
  toJSVal = toJSVal_generic (drop $ T.length "_splitArea_")

instance ToJSON SplitArea where
  toJSON = genericToJSON $ defaultOptions
    { fieldLabelModifier = drop $ T.length "_splitArea_"
    , omitNothingFields = True
    }
  toEncoding = genericToEncoding $ defaultOptions
    { fieldLabelModifier = drop $ T.length "_splitArea_"
    , omitNothingFields = True
    }

instance ToJSVal InOutOfRange where
  toJSVal = toJSVal_generic (drop $ T.length "_inOutOfRange_")

instance ToJSON InOutOfRange where
  toJSON = genericToJSON $ defaultOptions
    { fieldLabelModifier = drop $ T.length "_inOutOfRange_"
    , omitNothingFields = True
    }
  toEncoding = genericToEncoding $ defaultOptions
    { fieldLabelModifier = drop $ T.length "_inOutOfRange_"
    , omitNothingFields = True
    }
