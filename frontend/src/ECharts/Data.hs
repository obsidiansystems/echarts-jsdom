{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
module ECharts.Data
  where

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
import Data.Proxy
import Control.Lens
import ECharts.Types
import ECharts.Data.Internal

data Data seriesType = Data
  -- Common options
  { _data_name                   :: DataOptions_name seriesType
  , _data_value                  :: DataOptions_value seriesType
  -- , _data_symbol                 :: DataOptions_symbol seriesType
  }
  deriving (Generic)

makeLenses ''Data

instance Default (Data SeriesLine) where

instance ToJSON (Data SeriesLine) where
  toJSON = genericToJSON $ defaultOptions
    { fieldLabelModifier = drop $ T.length "_data_"
    , omitNothingFields = True
    }
  toEncoding = genericToEncoding $ defaultOptions
    { fieldLabelModifier = drop $ T.length "_data_"
    , omitNothingFields = True
    }

instance ToJSON (Data SeriesPie) where

