{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
module ECharts.Series.Internal where

import Control.Monad (join, void)
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
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Vector as V
import JSDOM.Types (JSVal, toJSVal, JSM, MonadJSM, liftJSM)
import Data.Proxy

import Reflex.Class (ffor)
import ECharts.Types

class SeriesOptions seriesType where
  getSeriesTypeInt :: Proxy seriesType -> Text
  type SeriesOptions_coordinateSystem seriesType
  type SeriesOptions_xAxisIndex seriesType
  type SeriesOptions_yAxisIndex seriesType
  type SeriesOptions_polarIndex seriesType
  type SeriesOptions_symbol seriesType
  type SeriesOptions_symbolSize seriesType
  type SeriesOptions_symbolRotate seriesType
  type SeriesOptions_symbolKeepAspect seriesType
  type SeriesOptions_symbolOffset seriesType
  type SeriesOptions_showSymbol seriesType
  type SeriesOptions_showAllSymbol seriesType
  type SeriesOptions_hoverAnimation seriesType
  type SeriesOptions_legendHoverLink seriesType
  type SeriesOptions_stack seriesType
  type SeriesOptions_cursor seriesType
  type SeriesOptions_connectNulls seriesType
  type SeriesOptions_clipOverflow seriesType
  type SeriesOptions_step seriesType
  type SeriesOptions_label seriesType
  type SeriesOptions_itemStyle seriesType
  type SeriesOptions_lineStyle seriesType
  type SeriesOptions_areaStyle seriesType
  type SeriesOptions_emphasis seriesType
  type SeriesOptions_smooth seriesType
  type SeriesOptions_smoothMonotone seriesType
  type SeriesOptions_sampling seriesType
  type SeriesOptions_dimensions seriesType
  type SeriesOptions_encode seriesType
  type SeriesOptions_seriesLayoutBy seriesType
  type SeriesOptions_datasetindex seriesType
  type SeriesOptions_markPoint seriesType
  type SeriesOptions_markLine seriesType
  type SeriesOptions_markArea seriesType
  type SeriesOptions_zlevel seriesType
  type SeriesOptions_z seriesType
  type SeriesOptions_animation seriesType
  type SeriesOptions_animationOptions seriesType
  type SeriesOptions_tooltip seriesType
  type SeriesOptions_barMinHeight seriesType
  type SeriesOptions_barGap seriesType
  type SeriesOptions_barCategoryGap seriesType
  type SeriesOptions_large seriesType
  type SeriesOptions_largeThreshold seriesType
  type SeriesOptions_progressive seriesType
  type SeriesOptions_progressiveThreshold seriesType
  type SeriesOptions_progressiveChunkMode seriesType
  type SeriesOptions_hoverOffset seriesType
  type SeriesOptions_selectedMode seriesType
  type SeriesOptions_selectedOffset seriesType
  type SeriesOptions_clockwise seriesType
  type SeriesOptions_minAngle seriesType
  type SeriesOptions_roseType seriesType
  type SeriesOptions_avoidLabelOverlap seriesType
  type SeriesOptions_stillShowZeroSum seriesType
  type SeriesOptions_center seriesType
  type SeriesOptions_radius seriesType
  type SeriesOptions_geoIndex seriesType
  type SeriesOptions_calendarIndex seriesType
  type SeriesOptions_showEffectOn seriesType
  type SeriesOptions_rippleEffect seriesType
  type SeriesOptions_radarIndex seriesType
  type SeriesOptions_left seriesType
  type SeriesOptions_top seriesType
  type SeriesOptions_right seriesType
  type SeriesOptions_bottom seriesType
  type SeriesOptions_width seriesType
  type SeriesOptions_height seriesType
  type SeriesOptions_layout seriesType
  type SeriesOptions_orient seriesType
  type SeriesOptions_roam seriesType
  type SeriesOptions_expandAndCollapse seriesType
  type SeriesOptions_initialTreeDepth seriesType
  type SeriesOptions_leaves seriesType
  type SeriesOptions_squareRatio seriesType
  type SeriesOptions_leafDepth seriesType
  type SeriesOptions_drillDownIcon seriesType
  type SeriesOptions_nodeClick seriesType
  type SeriesOptions_zoomToNodeRatio seriesType
  type SeriesOptions_levels seriesType
  type SeriesOptions_silent seriesType
  type SeriesOptions_visualDimension seriesType
  type SeriesOptions_visualMin seriesType
  type SeriesOptions_visualMax seriesType
  type SeriesOptions_colorAlpha seriesType
  type SeriesOptions_colorSaturation seriesType
  type SeriesOptions_colorMappingsBy seriesType
  type SeriesOptions_visibleMin seriesType
  type SeriesOptions_childrenVisibleMin seriesType
  type SeriesOptions_breadcrumb seriesType
  type SeriesOptions_sort seriesType
  type SeriesOptions_renderLabelForZeroData seriesType
  type SeriesOptions_downplay seriesType
  type SeriesOptions_boxWidth seriesType
  type SeriesOptions_barWidth seriesType
  type SeriesOptions_barMinWidth seriesType
  type SeriesOptions_barMaxWidth seriesType
  type SeriesOptions_blurSize seriesType
  type SeriesOptions_minOpacity seriesType
  type SeriesOptions_maxOpacity seriesType
  type SeriesOptions_aspectScale seriesType
  type SeriesOptions_boundingCoords seriesType
  type SeriesOptions_zoom seriesType
  type SeriesOptions_scaleLimit seriesType
  type SeriesOptions_nameMap seriesType
  type SeriesOptions_layoutCenter seriesType
  type SeriesOptions_mapValueCalculation seriesType
  type SeriesOptions_showLegendSymbol seriesType
  type SeriesOptions_parallelIndex seriesType
  type SeriesOptions_inactiveOpacity seriesType
  type SeriesOptions_activeOpacity seriesType
  type SeriesOptions_realtime seriesType
  type SeriesOptions_polyline seriesType
  type SeriesOptions_circularRotateLayout seriesType
  type SeriesOptions_force seriesType
  type SeriesOptions_nodeScaleRatio seriesType
  type SeriesOptions_draggable seriesType
  type SeriesOptions_edgeSymbol seriesType
  type SeriesOptions_edgeSymbolSize seriesType
  type SeriesOptions_categories seriesType
  type SeriesOptions_links seriesType
  type SeriesOptions_nodeWidth seriesType
  type SeriesOptions_nodeGap seriesType
  type SeriesOptions_layoutIterations seriesType
  type SeriesOptions_focusNodeAdjacency seriesType
  type SeriesOptions_min seriesType
  type SeriesOptions_max seriesType
  type SeriesOptions_minSize seriesType
  type SeriesOptions_maxSize seriesType
  type SeriesOptions_gap seriesType
  type SeriesOptions_funnelAlign seriesType
  type SeriesOptions_labelLine seriesType
  type SeriesOptions_startAngle seriesType
  type SeriesOptions_endAngle seriesType
  type SeriesOptions_splitNumber seriesType
  type SeriesOptions_axisLine seriesType
  type SeriesOptions_splitLine seriesType
  type SeriesOptions_axisTick seriesType
  type SeriesOptions_axisLabel seriesType
  type SeriesOptions_pointer seriesType
  type SeriesOptions_details seriesType

  -- By default use ()
  type instance SeriesOptions_coordinateSystem seriesType = ()
  type instance SeriesOptions_xAxisIndex seriesType = ()
  type instance SeriesOptions_yAxisIndex seriesType = ()
  type instance SeriesOptions_polarIndex seriesType = ()
  type instance SeriesOptions_symbol seriesType = ()
  type instance SeriesOptions_symbolSize seriesType = ()
  type instance SeriesOptions_symbolRotate seriesType = ()
  type instance SeriesOptions_symbolKeepAspect seriesType = ()
  type instance SeriesOptions_symbolOffset seriesType = ()
  type instance SeriesOptions_showSymbol seriesType = ()
  type instance SeriesOptions_showAllSymbol seriesType = ()
  type instance SeriesOptions_hoverAnimation seriesType = ()
  type instance SeriesOptions_legendHoverLink seriesType = ()
  type instance SeriesOptions_stack seriesType = ()
  type instance SeriesOptions_cursor seriesType = ()
  type instance SeriesOptions_connectNulls seriesType = ()
  type instance SeriesOptions_clipOverflow seriesType = ()
  type instance SeriesOptions_step seriesType = ()
  type instance SeriesOptions_label seriesType = ()
  type instance SeriesOptions_itemStyle seriesType = ()
  type instance SeriesOptions_lineStyle seriesType = ()
  type instance SeriesOptions_areaStyle seriesType = ()
  type instance SeriesOptions_emphasis seriesType = ()
  type instance SeriesOptions_smooth seriesType = ()
  type instance SeriesOptions_smoothMonotone seriesType = ()
  type instance SeriesOptions_sampling seriesType = ()
  type instance SeriesOptions_dimensions seriesType = ()
  type instance SeriesOptions_encode seriesType = ()
  type instance SeriesOptions_seriesLayoutBy seriesType = ()
  type instance SeriesOptions_datasetindex seriesType = ()
  type instance SeriesOptions_markPoint seriesType = ()
  type instance SeriesOptions_markLine seriesType = ()
  type instance SeriesOptions_markArea seriesType = ()
  type instance SeriesOptions_zlevel seriesType = ()
  type instance SeriesOptions_z seriesType = ()
  type instance SeriesOptions_animation seriesType = ()
  type instance SeriesOptions_animationOptions seriesType = ()
  type instance SeriesOptions_tooltip seriesType = ()
  type instance SeriesOptions_barMinHeight seriesType = ()
  type instance SeriesOptions_barGap seriesType = ()
  type instance SeriesOptions_barCategoryGap seriesType = ()
  type instance SeriesOptions_large seriesType = ()
  type instance SeriesOptions_largeThreshold seriesType = ()
  type instance SeriesOptions_progressive seriesType = ()
  type instance SeriesOptions_progressiveThreshold seriesType = ()
  type instance SeriesOptions_progressiveChunkMode seriesType = ()
  type instance SeriesOptions_hoverOffset seriesType = ()
  type instance SeriesOptions_selectedMode seriesType = ()
  type instance SeriesOptions_selectedOffset seriesType = ()
  type instance SeriesOptions_clockwise seriesType = ()
  type instance SeriesOptions_minAngle seriesType = ()
  type instance SeriesOptions_roseType seriesType = ()
  type instance SeriesOptions_avoidLabelOverlap seriesType = ()
  type instance SeriesOptions_stillShowZeroSum seriesType = ()
  type instance SeriesOptions_center seriesType = ()
  type instance SeriesOptions_radius seriesType = ()
  type instance SeriesOptions_geoIndex seriesType = ()
  type instance SeriesOptions_calendarIndex seriesType = ()
  type instance SeriesOptions_showEffectOn seriesType = ()
  type instance SeriesOptions_rippleEffect seriesType = ()
  type instance SeriesOptions_radarIndex seriesType = ()
  type instance SeriesOptions_left seriesType = ()
  type instance SeriesOptions_top seriesType = ()
  type instance SeriesOptions_right seriesType = ()
  type instance SeriesOptions_bottom seriesType = ()
  type instance SeriesOptions_width seriesType = ()
  type instance SeriesOptions_height seriesType = ()
  type instance SeriesOptions_layout seriesType = ()
  type instance SeriesOptions_orient seriesType = ()
  type instance SeriesOptions_roam seriesType = ()
  type instance SeriesOptions_expandAndCollapse seriesType = ()
  type instance SeriesOptions_initialTreeDepth seriesType = ()
  type instance SeriesOptions_leaves seriesType = ()
  type instance SeriesOptions_squareRatio seriesType = ()
  type instance SeriesOptions_leafDepth seriesType = ()
  type instance SeriesOptions_drillDownIcon seriesType = ()
  type instance SeriesOptions_nodeClick seriesType = ()
  type instance SeriesOptions_zoomToNodeRatio seriesType = ()
  type instance SeriesOptions_levels seriesType = ()
  type instance SeriesOptions_silent seriesType = ()
  type instance SeriesOptions_visualDimension seriesType = ()
  type instance SeriesOptions_visualMin seriesType = ()
  type instance SeriesOptions_visualMax seriesType = ()
  type instance SeriesOptions_colorAlpha seriesType = ()
  type instance SeriesOptions_colorSaturation seriesType = ()
  type instance SeriesOptions_colorMappingsBy seriesType = ()
  type instance SeriesOptions_visibleMin seriesType = ()
  type instance SeriesOptions_childrenVisibleMin seriesType = ()
  type instance SeriesOptions_breadcrumb seriesType = ()
  type instance SeriesOptions_sort seriesType = ()
  type instance SeriesOptions_renderLabelForZeroData seriesType = ()
  type instance SeriesOptions_downplay seriesType = ()
  type instance SeriesOptions_boxWidth seriesType = ()
  type instance SeriesOptions_barWidth seriesType = ()
  type instance SeriesOptions_barMinWidth seriesType = ()
  type instance SeriesOptions_barMaxWidth seriesType = ()
  type instance SeriesOptions_blurSize seriesType = ()
  type instance SeriesOptions_minOpacity seriesType = ()
  type instance SeriesOptions_maxOpacity seriesType = ()
  type instance SeriesOptions_aspectScale seriesType = ()
  type instance SeriesOptions_boundingCoords seriesType = ()
  type instance SeriesOptions_zoom seriesType = ()
  type instance SeriesOptions_scaleLimit seriesType = ()
  type instance SeriesOptions_nameMap seriesType = ()
  type instance SeriesOptions_layoutCenter seriesType = ()
  type instance SeriesOptions_mapValueCalculation seriesType = ()
  type instance SeriesOptions_showLegendSymbol seriesType = ()
  type instance SeriesOptions_parallelIndex seriesType = ()
  type instance SeriesOptions_inactiveOpacity seriesType = ()
  type instance SeriesOptions_activeOpacity seriesType = ()
  type instance SeriesOptions_realtime seriesType = ()
  type instance SeriesOptions_polyline seriesType = ()
  type instance SeriesOptions_circularRotateLayout seriesType = ()
  type instance SeriesOptions_force seriesType = ()
  type instance SeriesOptions_nodeScaleRatio seriesType = ()
  type instance SeriesOptions_draggable seriesType = ()
  type instance SeriesOptions_edgeSymbol seriesType = ()
  type instance SeriesOptions_edgeSymbolSize seriesType = ()
  type instance SeriesOptions_categories seriesType = ()
  type instance SeriesOptions_links seriesType = ()
  type instance SeriesOptions_nodeWidth seriesType = ()
  type instance SeriesOptions_nodeGap seriesType = ()
  type instance SeriesOptions_layoutIterations seriesType = ()
  type instance SeriesOptions_focusNodeAdjacency seriesType = ()
  type instance SeriesOptions_min seriesType = ()
  type instance SeriesOptions_max seriesType = ()
  type instance SeriesOptions_minSize seriesType = ()
  type instance SeriesOptions_maxSize seriesType = ()
  type instance SeriesOptions_gap seriesType = ()
  type instance SeriesOptions_funnelAlign seriesType = ()
  type instance SeriesOptions_labelLine seriesType = ()
  type instance SeriesOptions_startAngle seriesType = ()
  type instance SeriesOptions_endAngle seriesType = ()
  type instance SeriesOptions_splitNumber seriesType = ()
  type instance SeriesOptions_axisLine seriesType = ()
  type instance SeriesOptions_splitLine seriesType = ()
  type instance SeriesOptions_axisTick seriesType = ()
  type instance SeriesOptions_axisLabel seriesType = ()
  type instance SeriesOptions_pointer seriesType = ()
  type instance SeriesOptions_details seriesType = ()

instance SeriesOptions SeriesLine where
  getSeriesTypeInt _ = "line"
  type SeriesOptions_stack SeriesLine = Maybe Text
  type SeriesOptions_smooth SeriesLine = Maybe (Either Bool ZeroToOne)
  type SeriesOptions_areaStyle SeriesLine = Maybe AreaStyle
  type SeriesOptions_label SeriesLine = Maybe Label
  type SeriesOptions_lineStyle SeriesLine = Maybe LineStyle
  type SeriesOptions_markArea SeriesLine = Maybe MarkArea
  type SeriesOptions_yAxisIndex SeriesLine = Maybe Int
  type SeriesOptions_animation SeriesLine = Maybe Bool
  type SeriesOptions_itemStyle SeriesLine = Maybe ItemStyle
  type SeriesOptions_symbol SeriesLine = Maybe Symbol
  type SeriesOptions_symbolSize SeriesLine = Maybe SymbolSize
  type SeriesOptions_showSymbol SeriesLine = Maybe Bool
  type SeriesOptions_hoverAnimation SeriesLine = Maybe Bool

instance SeriesOptions SeriesPie where
  getSeriesTypeInt _ = "pie"
  type SeriesOptions_selectedMode SeriesPie = Maybe SelectedMode
