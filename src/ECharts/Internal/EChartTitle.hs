{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module ECharts.Internal.EChartTitle where

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
import ECharts.DeriveToJSVal (toJSVal_generic, ToJSVal(..))

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

instance ToJSVal EChartTitle where
  toJSVal = toJSVal_generic (drop $ T.length "_eChartTitle_")
