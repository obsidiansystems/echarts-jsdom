{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module ECharts.Internal.EChartLegend where

import Data.Aeson (ToJSON, genericToEncoding, genericToJSON, defaultOptions, Options(..))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Scientific
import Data.Time
import GHC.Generics (Generic)
import Data.Default (Default, def)
import qualified Data.Aeson as Aeson

import ECharts.Types
import ECharts.Internal.EChartTypes
import ECharts.Series
import ECharts.ChartOptions

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
