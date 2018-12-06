{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module ECharts.Internal.EChartToolTip where

import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import qualified Data.Aeson as Aeson

import ECharts.DeriveToJSVal (toJSVal_generic, ToJSVal(..))
import ECharts.Types
import ECharts.Internal.EChartTypes

data EChartToolTip = EChartToolTip
  { _eChartToolTip_show :: Maybe Bool
  , _eChartToolTip_trigger :: Maybe Text
  , _eChartToolTip_axisPointer :: Maybe AxisPointer
  , _eChartToolTip_showContent :: Maybe Bool
  , _eChartToolTip_alwaysShowContent :: Maybe Bool
  , _eChartToolTip_triggerOn :: Maybe Text
  , _eChartToolTip_showDelay :: Maybe Int
  , _eChartToolTip_hideDelay :: Maybe Int
  , _eChartToolTip_enterable :: Maybe Bool
  , _eChartToolTip_confine :: Maybe Bool
  , _eChartToolTip_transitionDuration :: Maybe Double
  , _eChartToolTip_position :: Maybe Aeson.Value
  , _eChartToolTip_formatter :: Maybe Aeson.Value
  , _eChartToolTip_backgroundColor :: Maybe Color
  , _eChartToolTip_borderColor :: Maybe Color
  , _eChartToolTip_borderWidth :: Maybe Int
  , _eChartToolTip_padding :: Maybe [Int]
  , _eChartToolTip_textStyle :: Maybe EChartTextStyle
  , _eChartToolTip_extraCssText :: Maybe Text
  }
  deriving (Generic)

instance ToJSVal EChartToolTip where
  toJSVal = toJSVal_generic (drop $ T.length "_eChartToolTip_")

instance ToJSVal ToolTip where
  toJSVal = toJSVal . toEChartToolTip

toEChartToolTip :: ToolTip -> EChartToolTip
toEChartToolTip v = EChartToolTip
  { _eChartToolTip_show = _toolTip_show v
  , _eChartToolTip_trigger = _toolTip_trigger v
  , _eChartToolTip_axisPointer = _toolTip_axisPointer v
  , _eChartToolTip_showContent = _toolTip_showContent v
  , _eChartToolTip_alwaysShowContent = _toolTip_alwaysShowContent v
  , _eChartToolTip_triggerOn = _toolTip_triggerOn v
  , _eChartToolTip_showDelay = _toolTip_showDelay v
  , _eChartToolTip_hideDelay = _toolTip_hideDelay v
  , _eChartToolTip_enterable = _toolTip_enterable v
  , _eChartToolTip_confine = _toolTip_confine v
  , _eChartToolTip_transitionDuration = _toolTip_transitionDuration  v
  , _eChartToolTip_position = _toolTip_position v
  , _eChartToolTip_formatter = _toolTip_formatter v
  , _eChartToolTip_backgroundColor = _toolTip_backgroundColor v
  , _eChartToolTip_borderColor =  _border_color =<< _toolTip_border v
  , _eChartToolTip_borderWidth =  _border_width =<< _toolTip_border v
  , _eChartToolTip_padding = _toolTip_padding v
  , _eChartToolTip_textStyle = toEChartTextStyle <$> _toolTip_textStyle v
  , _eChartToolTip_extraCssText = _toolTip_extraCssText v
  }
