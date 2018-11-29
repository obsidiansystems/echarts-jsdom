{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
module Frontend where

import Control.Monad (join, void)
import Control.Monad.Fix (MonadFix)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (ToJSON, genericToEncoding, genericToJSON, defaultOptions, Options(..))
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LBS
import Data.Default (Default, def)
import qualified Data.HashMap.Strict as HashMap
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Scientific
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time
import Data.Time.ISO8601
import qualified Data.Vector as V
import GHC.Generics (Generic)
import GHCJS.DOM.Types (Element)
import JSDOM.Types (JSVal, toJSVal, JSM, MonadJSM, liftJSM)
import Language.Javascript.JSaddle.Evaluate
import Language.Javascript.JSaddle.Object
import Network.URI (parseURI, URI(..), URIAuth(..))
import qualified Obelisk.ExecutableConfig
import Obelisk.Frontend
import Obelisk.Route
import Reflex.Dom.Core
import qualified Data.Some as Some
import Control.Lens
import Common.Types
import Common.Route
import Obelisk.Generated.Static
import System.Random

import Debug.Trace

import ECharts
import ECharts.Internal
import ECharts.Types
import ECharts.Series
import ECharts.Data
import ECharts.ChartOptions
import ECharts.Examples

frontend :: Frontend (R FrontendRoute)
frontend = Frontend
  { _frontend_head = do
    el "title" $ text "Obelisk Minimal Example"
    elAttr "meta" ("charset" =: "utf-8") blank
    elAttr "script" ("type" =: "text/javascript" <> "src" =: static @"echarts.min.js") blank
  , _frontend_body = do
    Just r <- liftIO $ Obelisk.ExecutableConfig.get "config/common/route"
    let Just (URI scheme (Just auth)  _ _ _) = parseURI $ T.unpack $ T.strip r
        wsScheme = case scheme of
          "https:" -> "wss:"
          _ -> "ws:"
        wsUrl = T.pack $ wsScheme <> (uriRegName auth) <> (uriPort auth) <> "/listen"
    -- prerender blank (echarts wsUrl >> seriesExamples)
    prerender blank $ do
      pb <- getPostBuild
      dEv <- getAndDecode $ (static @"data/confidence-band.json") <$ pb
      widgetHold blank $ ffor dEv $ \(Just d) ->
        (seriesExamples (mkStdGen 0) d)
      blank
  }

echarts
  :: forall t m.
     ( DomBuilder t m
     , PostBuild t m
     , PerformEvent t m
     , MonadJSM (Performable m)
     , GhcjsDomSpace ~ DomBuilderSpace m
     , MonadHold t m
     , MonadFix m
     , TriggerEvent t m
     , HasJSContext m
     , MonadJSM m
     )
  => Text
  -> m ()
echarts wsUrl = el "main" $ do
  ws <- webSocket wsUrl $ def & webSocketConfig_send .~ (never :: Event t [Text])
  receivedMessages :: Dynamic t [(UTCTime, CpuStat Scientific)] <- foldDyn (\m ms -> case Aeson.decode (LBS.fromStrict m) of
    Nothing -> ms
    Just m' -> take 50 $ m' : ms) [] $ _webSocket_recv ws
  let cpuStatMap (t, c) = mconcat
        [ "user" =: [(t, _cpuStat_user c)]
        , "nice" =: [(t, _cpuStat_nice c)]
        , "system" =: [(t, _cpuStat_system c)]
        , "idle" =: [(t, _cpuStat_idle c)]
        , "iowait" =: [(t, _cpuStat_iowait c)]
        , "irq" =: [(t, _cpuStat_irq c)]
        , "softirq" =: [(t, _cpuStat_softirq c)]
        , "steal" =: [(t, _cpuStat_steal c)]
        , "guest" =: [(t, _cpuStat_guest c)]
        , "guestNice" =: [(t, _cpuStat_guestNice c)]
        ]
  dynamicTimeSeries "Test" $ Map.unionsWith (++) . fmap cpuStatMap . reverse <$> receivedMessages

dynamicTimeSeries
  :: ( DomBuilder t m
     , PerformEvent t m
     , PostBuild t m
     , MonadHold t m
     , MonadJSM (Performable m)
     , GhcjsDomSpace ~ DomBuilderSpace m
     )
  => Text
  -> Dynamic t (Map Text [(UTCTime, Scientific)])
  -> m ()
dynamicTimeSeries title ts = do
  e <- fst <$> elAttr' "div" ("style" =: "width:600px; height:400px;") blank
  p <- getPostBuild
  chart <- performEvent $ ffor p $ \_ -> liftJSM $ ECharts.init $ _element_raw e
  let opts0 = def
        { _chartOptions_title = def { _title_text = Just title }
        , _chartOptions_xAxis = def { _axis_type = Just AxisType_Time } :[]
        , _chartOptions_yAxis = def { _axis_type = Just AxisType_Value
                                    , _axis_min = Just $ Left 0
                                    , _axis_max = Just $ Left 101
                                    } :[]
        , _chartOptions_series = []
        }
  performEvent_ $ ffor chart $ \c -> liftJSM $ setOption c opts0
  mchart <- holdDyn Nothing $ Just <$> chart
  let opts = leftmost
        [ attach (current mchart) $ updated ts
        , attachWith (\t c -> (c, t)) (current ts) (updated mchart)
        ]
  performEvent_ $ fforMaybe opts $ \case
    (Nothing, _) -> Nothing
    (Just c, ts') -> Just $ liftJSM $ setOption c $ opts0
      { _chartOptions_series = ffor (reverse $ Map.toList ts') $ \(k, vs) -> Some.This $
        SeriesT_Line $ def
          & series_name ?~ k
          & series_smooth ?~ Right (scientific 7 (-1))
          & series_data ?~ (ffor vs $ \(t, v) -> def
            & data_name ?~ (scientific (toInteger $ utcTimeToEpoch t) 0)
            & data_value ?~ (scientific (toInteger $ utcTimeToEpoch t) 0, v))
      }
