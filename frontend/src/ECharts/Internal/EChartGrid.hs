{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module ECharts.Internal.EChartGrid where

import Data.Aeson (ToJSON, genericToEncoding, genericToJSON, defaultOptions, Options(..))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Scientific
import Data.Time
import GHC.Generics (Generic)
import qualified Data.Aeson as Aeson
import Data.Default (Default, def)

import ECharts.Types
import ECharts.Internal.EChartTypes
import ECharts.Series
import ECharts.ChartOptions

data EChartGrid = EChartGrid
  { _eChartGrid_show :: Maybe Bool
  , _eChartGrid_id :: Maybe Text
  , _eChartGrid_zlevel :: Maybe Int
  , _eChartGrid_z :: Maybe Int
  , _eChartGrid_left :: Maybe SN
  , _eChartGrid_right :: Maybe SN
  , _eChartGrid_top :: Maybe SN
  , _eChartGrid_bottom :: Maybe SN
  , _eChartGrid_width :: Maybe SN
  , _eChartGrid_height :: Maybe SN
  , _eChartGrid_containLabel :: Maybe Bool
  , _eChartGrid_backgroundColor :: Maybe Text
  , _eChartGrid_borderColor :: Maybe Text
  , _eChartGrid_borderWidth :: Maybe Int
  , _eChartGrid_shadowBlur :: Maybe Int
  , _eChartGrid_shadowColor :: Maybe Text
  , _eChartGrid_shadowOffsetX :: Maybe Int
  , _eChartGrid_shadowOffsetY :: Maybe Int
  , _eChartGrid_tooltip :: Maybe Aeson.Value
  }
  deriving (Generic)

instance ToJSON EChartGrid where
  toJSON = genericToJSON $ defaultOptions
    { fieldLabelModifier = drop $ T.length "_eChartGrid_"
    , omitNothingFields = True
    }
  toEncoding = genericToEncoding $ defaultOptions
    { fieldLabelModifier = drop $ T.length "_eChartGrid_"
    , omitNothingFields = True
    }

toEChartGrid :: Grid -> EChartGrid
toEChartGrid g = EChartGrid
  { _eChartGrid_show = _grid_show g
  , _eChartGrid_id = Nothing
  , _eChartGrid_zlevel = _position_zlevel =<< _grid_position g
  , _eChartGrid_z = _position_z =<< _grid_position g
  , _eChartGrid_left = posAlignToSN <$> (_position_left =<< _grid_position g)
  , _eChartGrid_right = posAlignToSN <$> (_position_right =<< _grid_position g)
  , _eChartGrid_top = posAlignToSN <$> (_position_top =<< _grid_position g)
  , _eChartGrid_bottom = posAlignToSN <$> (_position_bottom =<< _grid_position g)
  , _eChartGrid_width = sizeValueToSN <$> (_size_width =<< _grid_size g)
  , _eChartGrid_height = sizeValueToSN <$> (_size_height =<< _grid_size g)
  , _eChartGrid_containLabel = _grid_containLabel g
  , _eChartGrid_backgroundColor = _grid_backgroundColor g
  , _eChartGrid_borderColor =  _border_color =<< _grid_border g
  , _eChartGrid_borderWidth =  _border_width =<< _grid_border g
  , _eChartGrid_shadowBlur =  _shadow_blur =<< _grid_shadow g
  , _eChartGrid_shadowColor =  _shadow_color =<< _grid_shadow g
  , _eChartGrid_shadowOffsetX =  _shadow_offsetX =<< _grid_shadow g
  , _eChartGrid_shadowOffsetY =  _shadow_offsetY =<< _grid_shadow g
  , _eChartGrid_tooltip = Nothing
  }
