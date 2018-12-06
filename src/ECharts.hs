module ECharts where

import ECharts.Types
import ECharts.ChartOptions
import ECharts.Internal

import Control.Monad (join, void)
import Control.Monad.IO.Class
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Aeson as Aeson
import JSDOM.Types (JSVal, toJSVal, JSM, MonadJSM, liftJSM)
import Language.Javascript.JSaddle.Evaluate
import Language.Javascript.JSaddle.Object
import Language.Javascript.JSaddle
import GHCJS.DOM.Types (Element)

init :: GHCJS.DOM.Types.Element -> JSM ECharts
init e = do
  f <- eval $ T.pack "(function(e) { return echarts['init'](e) })"
  arg <- toJSVal e
  ECharts <$> call f f [arg]

setOption :: ECharts -> ChartOptions -> JSM ()
setOption c opts = do
  f <- eval $ T.pack "(function(e, opt) { e['setOption'](opt); })"
  let chart = unECharts c
  options <- toJSVal =<< toEChartConfig opts
  void $ call f f [chart, options]
