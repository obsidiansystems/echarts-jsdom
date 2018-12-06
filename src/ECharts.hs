module ECharts
  ( initECharts
  , setOption
  , module X
  , ECharts
  )
  where

import ECharts.Types as X
import ECharts.ChartOptions as X
import ECharts.Series as X
import ECharts.Data as X
import ECharts.Internal

import Control.Monad (void)
import qualified Data.Text as T
import Language.Javascript.JSaddle
import GHCJS.DOM.Types (Element)

initECharts :: GHCJS.DOM.Types.Element -> JSM ECharts
initECharts e = do
  f <- eval $ T.pack "(function(e) { return echarts['init'](e) })"
  arg <- toJSVal e
  ECharts <$> call f f [arg]

setOption :: ECharts -> ChartOptions -> JSM ()
setOption c opts = do
  f <- eval $ T.pack "(function(e, opt) { e['setOption'](opt); })"
  let chart = unECharts c
  options <- toJSVal =<< toEChartConfig opts
  void $ call f f [chart, options]
