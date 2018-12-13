{-# LANGUAGE FlexibleInstances #-}
module ECharts
  ( initECharts
  , setOption
  , setOptionJSVal
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
import Language.Javascript.JSaddle
import GHCJS.DOM.Types (Element)
import Data.Some (Some)

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

instance ToJSVal ChartOptions where
  toJSVal o = toJSVal =<< toEChartConfig o

instance ToJSVal (Some SeriesT) where
  toJSVal o = toJSVal =<< toEChartSeries o
