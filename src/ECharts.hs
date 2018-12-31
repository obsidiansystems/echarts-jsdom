{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module ECharts
  ( initECharts
  , setOption
  , setOptionJSVal
  , onRenderedAction
  , onFinishedAction
  , module X
  , ECharts
  )
  where

import ECharts.Types as X
import ECharts.ChartOptions as X
import ECharts.Series as X
import ECharts.Data as X
import ECharts.Internal
import ECharts.Internal.EChartSeries

import Control.Monad (void)
import qualified Data.Text as T
import Language.Javascript.JSaddle (eval, call, function, fun, toJSVal)
import GHCJS.DOM.Types (Element)
import Data.Some (Some)
import GHCJS.DOM.EventM
import GHCJS.DOM.Types
import GHCJS.DOM.EventTargetClosures

data ECharts = ECharts { unECharts :: JSVal }

initECharts :: GHCJS.DOM.Types.Element -> JSM ECharts
initECharts e = do
  f <- eval $ T.pack "(function(e) { return echarts['init'](e) })"
  arg <- toJSVal e
  ECharts <$> call f f [arg]

setOption :: ECharts -> ChartOptions -> JSM ()
setOption c opts = do
  options <- toJSVal opts
  setOptionJSVal c options

setOptionJSVal :: ECharts -> JSVal -> JSM ()
setOptionJSVal c options = do
  f <- eval $ T.pack "(function(e, opt) { e['setOption'](opt); })"
  let chart = unECharts c
  void $ call f f [chart, options]

onRenderedAction :: ECharts -> (JSM ()) -> JSM ()
onRenderedAction c action = do
  jsF <- toJSVal =<< function (fun $ \_ _ _ -> action)
  f <- eval $ T.pack "(function(e, opt) { e['on']('rendered', opt); })"
  let chart = unECharts c
  void $ call f f [chart, jsF]

onFinishedAction :: ECharts -> (JSM ()) -> JSM ()
onFinishedAction c action = do
  jsF <- toJSVal =<< function (fun $ \_ _ _ -> action)
  f <- eval $ T.pack "(function(e, opt) { e['on']('finished', opt); })"
  let chart = unECharts c
  void $ call f f [chart, jsF]

instance ToJSVal ChartOptions where
  toJSVal o = toJSVal =<< toEChartConfig o

instance ToJSVal (Some SeriesT) where
  toJSVal o = toJSVal =<< toEChartSeries o
