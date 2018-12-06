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

import qualified Language.Javascript.JSaddle.Object as OI (Object(..), create, setProp, getProp)
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
import ECharts.DeriveToJSVal (toJSVal_generic, ToJSVal(..))

data Data seriesType = DataObject
  -- Common options
  { _data_name                   :: DataOptions_name seriesType
  , _data_value                  :: DataOptions_value seriesType
  -- , _data_symbol                 :: DataOptions_symbol seriesType
  }
  | DataInt Int
  | DataDouble Double
  | DataText Text
  deriving (Generic)

makeLenses ''Data

instance Default (Data SeriesLine) where
  def = DataObject def def

instance ToJSVal (Data SeriesPie) where
  toJSVal = \case
    (DataInt a) -> toJSVal a
    (DataDouble a) -> toJSVal a
    (DataText a) -> toJSVal a
    (DataObject a b) -> do
      o <- OI.create
      aO <- toJSVal a
      bO <- toJSVal b
      OI.setProp "name" aO o
      OI.setProp "value" bO o
      toJSVal o

instance ToJSVal (Data SeriesLine) where
  toJSVal = \case
    (DataInt a) -> toJSVal a
    (DataDouble a) -> toJSVal a
    (DataText a) -> toJSVal a
    (DataObject a b) -> do
      o <- OI.create
      aO <- toJSVal a
      bO <- toJSVal b
      OI.setProp "name" aO o
      OI.setProp "value" bO o
      toJSVal o

instance ToJSON (Data SeriesLine) where
  toJSON = genericToJSON $ defaultOptions
    { fieldLabelModifier = drop $ T.length "_data_"
    , omitNothingFields = True
    , sumEncoding = Aeson.UntaggedValue
    }
  toEncoding = genericToEncoding $ defaultOptions
    { fieldLabelModifier = drop $ T.length "_data_"
    , omitNothingFields = True
    , sumEncoding = Aeson.UntaggedValue
    }

instance ToJSON (Data SeriesPie) where
