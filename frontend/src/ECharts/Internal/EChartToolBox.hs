{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module ECharts.Internal.EChartToolBox where

import Data.Aeson (ToJSON, genericToEncoding, genericToJSON, defaultOptions, Options(..))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Scientific
import Data.Time
import GHC.Generics (Generic)
import qualified Data.Aeson as Aeson
import Data.Default (Default, def)
import qualified Data.HashMap.Strict as HashMap

import Reflex.Class (ffor)

import ECharts.Types
import ECharts.Internal.EChartTypes
import ECharts.Series
import ECharts.ChartOptions

data EChartToolBox = EChartToolBox
  { _eChartToolBox_show :: Maybe Bool
  , _eChartToolBox_id :: Maybe Text
  , _eChartToolBox_orient :: Maybe Orientation
  , _eChartToolBox_itemSize :: Maybe Int
  , _eChartToolBox_itemGap :: Maybe Int
  , _eChartToolBox_showTitle :: Maybe Bool
  , _eChartToolBox_feature :: Maybe Aeson.Value
  , _eChartToolBox_iconStyle :: Maybe EChartIconStyle
  , _eChartToolBox_zlevel :: Maybe Int
  , _eChartToolBox_z :: Maybe Int
  , _eChartToolBox_left :: Maybe SN
  , _eChartToolBox_right :: Maybe SN
  , _eChartToolBox_top :: Maybe SN
  , _eChartToolBox_bottom :: Maybe SN
  , _eChartToolBox_width :: Maybe SN
  , _eChartToolBox_height :: Maybe SN
  }
  deriving (Generic)

instance ToJSON EChartToolBox where
  toJSON = genericToJSON $ defaultOptions
    { fieldLabelModifier = drop $ T.length "_eChartToolBox_"
    , omitNothingFields = True
    }
  toEncoding = genericToEncoding $ defaultOptions
    { fieldLabelModifier = drop $ T.length "_eChartToolBox_"
    , omitNothingFields = True
    }

toEChartToolBox :: ToolBox -> EChartToolBox
toEChartToolBox v = EChartToolBox
  { _eChartToolBox_show = _toolBox_show v
  , _eChartToolBox_id = _toolBox_id v
  , _eChartToolBox_orient = _toolBox_orient v
  , _eChartToolBox_itemSize = _toolBox_itemSize v
  , _eChartToolBox_itemGap = _toolBox_itemGap v
  , _eChartToolBox_showTitle = _toolBox_showTitle v
  , _eChartToolBox_feature = if (null $ _toolBox_features v) then Nothing else Just features
  , _eChartToolBox_iconStyle = _toolBox_iconStyle v
  , _eChartToolBox_zlevel = _position_zlevel =<< _toolBox_position v
  , _eChartToolBox_z = _position_z =<< _toolBox_position v
  , _eChartToolBox_left = posAlignToSN <$> (_position_left =<< _toolBox_position v)
  , _eChartToolBox_right = posAlignToSN <$> (_position_right =<< _toolBox_position v)
  , _eChartToolBox_top = posAlignToSN <$> (_position_top =<< _toolBox_position v)
  , _eChartToolBox_bottom = posAlignToSN <$> (_position_bottom =<< _toolBox_position v)
  , _eChartToolBox_width = sizeValueToSN <$> (_size_width =<< _toolBox_size v)
  , _eChartToolBox_height = sizeValueToSN <$> (_size_height =<< _toolBox_size v)
  }
  where
    features = Aeson.Object $ HashMap.fromList $ ffor (_toolBox_features v) $ \f -> case f of
      (Feature_SaveAsImage _ _ _ _ _ _ _ _) -> ("saveAsImage", ob)
        where
          ob = Aeson.toJSON f
