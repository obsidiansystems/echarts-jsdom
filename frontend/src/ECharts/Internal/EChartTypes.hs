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

import ECharts.Types

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
