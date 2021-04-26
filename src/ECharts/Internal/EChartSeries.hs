{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
module ECharts.Internal.EChartSeries where

import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Data.Default (Default, def)
import Data.Some (Some)
import qualified Data.Some as Some
import Control.Lens
import Language.Javascript.JSaddle

import ECharts.DeriveToJSVal (toJSVal_generic, ToJSVal(..))
import ECharts.Series
import ECharts.Internal.EChartTypes ()
import ECharts.Internal.EChartToolTip ()

data EChartSeries = EChartSeries
  -- line options
  { _eChartSeries_type :: Maybe Text
  , _eChartSeries_id :: Maybe Text
  , _eChartSeries_name :: Maybe Text
  , _eChartSeries_coordinateSystem :: Maybe JSVal
  , _eChartSeries_xAxisIndex :: Maybe JSVal
  , _eChartSeries_yAxisIndex :: Maybe JSVal
  , _eChartSeries_polarIndex :: Maybe JSVal
  , _eChartSeries_symbol :: Maybe JSVal
  , _eChartSeries_symbolSize :: Maybe JSVal
  , _eChartSeries_symbolRotate :: Maybe JSVal
  , _eChartSeries_symbolKeepAspect :: Maybe JSVal
  , _eChartSeries_symbolOffset :: Maybe JSVal
  , _eChartSeries_showSymbol :: Maybe JSVal
  , _eChartSeries_showAllSymbol :: Maybe JSVal
  , _eChartSeries_hoverAnimation :: Maybe JSVal
  , _eChartSeries_legendHoverLink :: Maybe JSVal
  , _eChartSeries_stack :: Maybe JSVal
  , _eChartSeries_cursor :: Maybe JSVal
  , _eChartSeries_connectNulls :: Maybe JSVal
  , _eChartSeries_clipOverflow :: Maybe JSVal
  , _eChartSeries_step :: Maybe JSVal
  , _eChartSeries_label :: Maybe JSVal
  , _eChartSeries_itemStyle :: Maybe JSVal
  , _eChartSeries_lineStyle :: Maybe JSVal
  , _eChartSeries_areaStyle :: Maybe JSVal
  , _eChartSeries_emphasis :: Maybe JSVal
  , _eChartSeries_smooth :: Maybe JSVal
  , _eChartSeries_smoothMonotone :: Maybe JSVal
  , _eChartSeries_sampling :: Maybe JSVal
  , _eChartSeries_dimensions :: Maybe JSVal
  , _eChartSeries_encode :: Maybe JSVal
  , _eChartSeries_seriesLayoutBy :: Maybe JSVal
  , _eChartSeries_datasetIndex :: Maybe JSVal
  , _eChartSeries_data :: Maybe JSVal
  , _eChartSeries_markPoint :: Maybe JSVal
  , _eChartSeries_markLine :: Maybe JSVal
  , _eChartSeries_markArea :: Maybe JSVal
  , _eChartSeries_zlevel :: Maybe JSVal
  , _eChartSeries_z :: Maybe JSVal
  , _eChartSeries_animation :: Maybe JSVal
  , _eChartSeries_animationThreshold :: Maybe JSVal
  , _eChartSeries_animationDuration :: Maybe JSVal
  , _eChartSeries_animationEasing :: Maybe JSVal
  , _eChartSeries_animationDelay :: Maybe JSVal
  , _eChartSeries_animationDurationUpdate :: Maybe JSVal
  , _eChartSeries_animationEasingUpdate :: Maybe JSVal
  , _eChartSeries_animationDelayUpdate :: Maybe JSVal
  , _eChartSeries_tooltip :: Maybe JSVal
  -- bar options
  -- candlestick has AbsOrPercent
  -- , _eChartSeries_barWidth :: Maybe JSVal
  -- , _eChartSeries_barMaxWidth :: Maybe JSVal
  , _eChartSeries_barMinHeight :: Maybe JSVal
  , _eChartSeries_barGap :: Maybe JSVal
  , _eChartSeries_barCategoryGap :: Maybe JSVal
  , _eChartSeries_large :: Maybe JSVal
  , _eChartSeries_largeThreshold :: Maybe JSVal
  , _eChartSeries_progressive :: Maybe JSVal
  , _eChartSeries_progressiveThreshold :: Maybe JSVal
  , _eChartSeries_progressiveChunkMode :: Maybe JSVal
  -- pie options
  , _eChartSeries_hoverOffset :: Maybe JSVal
  , _eChartSeries_selectedMode :: Maybe JSVal
  , _eChartSeries_selectedOffset :: Maybe JSVal
  , _eChartSeries_clockwise :: Maybe JSVal
  , _eChartSeries_minAngle :: Maybe JSVal
  , _eChartSeries_roseType :: Maybe JSVal
  , _eChartSeries_avoidLabelOverlap :: Maybe JSVal
  , _eChartSeries_stillShowZeroSum :: Maybe JSVal
  , _eChartSeries_center :: Maybe JSVal
  , _eChartSeries_radius :: Maybe JSVal
  -- scatter options
  , _eChartSeries_geoIndex :: Maybe JSVal
  , _eChartSeries_calendarIndex :: Maybe JSVal
  -- effectScatter options
  , _eChartSeries_showEffectOn :: Maybe JSVal
  , _eChartSeries_rippleEffect :: Maybe JSVal
  -- radar options
  , _eChartSeries_radarIndex :: Maybe JSVal
  -- tree options
  , _eChartSeries_left :: Maybe JSVal
  , _eChartSeries_top :: Maybe JSVal
  , _eChartSeries_right :: Maybe JSVal
  , _eChartSeries_bottom :: Maybe JSVal
  , _eChartSeries_width :: Maybe JSVal
  , _eChartSeries_height :: Maybe JSVal
  , _eChartSeries_layout :: Maybe JSVal
  , _eChartSeries_orient :: Maybe JSVal
  , _eChartSeries_roam :: Maybe JSVal
  , _eChartSeries_expandAndCollapse :: Maybe JSVal
  , _eChartSeries_initialTreeDepth :: Maybe JSVal
  , _eChartSeries_leaves :: Maybe JSVal
  -- treemap options
  , _eChartSeries_squareRatio :: Maybe JSVal
  , _eChartSeries_leafDepth :: Maybe JSVal
  , _eChartSeries_drillDownIcon :: Maybe JSVal
  , _eChartSeries_nodeClick :: Maybe JSVal
  , _eChartSeries_zoomToNodeRatio :: Maybe JSVal
  , _eChartSeries_levels :: Maybe JSVal
  , _eChartSeries_silent :: Maybe JSVal
  , _eChartSeries_visualDimension :: Maybe JSVal
  , _eChartSeries_visualMin :: Maybe JSVal
  , _eChartSeries_visualMax :: Maybe JSVal
  , _eChartSeries_colorAlpha :: Maybe JSVal
  , _eChartSeries_colorSaturation :: Maybe JSVal
  , _eChartSeries_colorMappingsBy :: Maybe JSVal
  , _eChartSeries_visibleMin :: Maybe JSVal
  , _eChartSeries_childrenVisibleMin :: Maybe JSVal
  , _eChartSeries_breadcrumb :: Maybe JSVal
  -- sunburst options
  , _eChartSeries_sort :: Maybe JSVal
  , _eChartSeries_renderLabelForZeroData :: Maybe JSVal
  , _eChartSeries_downplay :: Maybe JSVal
  -- boxplot options
  , _eChartSeries_boxWidth :: Maybe JSVal
  -- candlestick options
  , _eChartSeries_barWidth :: Maybe JSVal
  , _eChartSeries_barMinWidth :: Maybe JSVal
  , _eChartSeries_barMaxWidth :: Maybe JSVal
  -- heatmap
  , _eChartSeries_blurSize :: Maybe JSVal
  , _eChartSeries_minOpacity :: Maybe JSVal
  , _eChartSeries_maxOpacity :: Maybe JSVal
  -- map
  , _eChartSeries_aspectScale :: Maybe JSVal
  , _eChartSeries_boundingCoords :: Maybe JSVal
  , _eChartSeries_zoom :: Maybe JSVal
  , _eChartSeries_scaleLimit :: Maybe JSVal
  , _eChartSeries_nameMap :: Maybe JSVal
  , _eChartSeries_layoutCenter :: Maybe JSVal
  , _eChartSeries_mapValueCalculation :: Maybe JSVal
  , _eChartSeries_showLegendSymbol :: Maybe JSVal
  -- parallel
  , _eChartSeries_parallelIndex :: Maybe JSVal
  , _eChartSeries_inactiveOpacity :: Maybe JSVal
  , _eChartSeries_activeOpacity :: Maybe JSVal
  , _eChartSeries_realtime :: Maybe JSVal
  -- lines
  , _eChartSeries_polyline :: Maybe JSVal
  -- graph
  , _eChartSeries_circularRotateLayout :: Maybe JSVal
  , _eChartSeries_force :: Maybe JSVal
  , _eChartSeries_nodeScaleRatio :: Maybe JSVal
  , _eChartSeries_draggable :: Maybe JSVal
  -- focusNodeAdjacency :: Maybe JSVal
  , _eChartSeries_edgeSymbol :: Maybe JSVal
  , _eChartSeries_edgeSymbolSize :: Maybe JSVal
  , _eChartSeries_categories :: Maybe JSVal
  , _eChartSeries_links :: Maybe JSVal
  -- sankey
  , _eChartSeries_nodeWidth :: Maybe JSVal
  , _eChartSeries_nodeGap :: Maybe JSVal
  , _eChartSeries_layoutIterations :: Maybe JSVal
  , _eChartSeries_focusNodeAdjacency :: Maybe JSVal
  -- funnel
  , _eChartSeries_min :: Maybe JSVal
  , _eChartSeries_max :: Maybe JSVal
  , _eChartSeries_minSize :: Maybe JSVal
  , _eChartSeries_maxSize :: Maybe JSVal
  , _eChartSeries_gap :: Maybe JSVal
  , _eChartSeries_funnelAlign :: Maybe JSVal
  , _eChartSeries_labelLine :: Maybe JSVal
  -- gauge
  , _eChartSeries_startAngle :: Maybe JSVal
  , _eChartSeries_endAngle :: Maybe JSVal
  , _eChartSeries_splitNumber :: Maybe JSVal
  , _eChartSeries_axisLine :: Maybe JSVal
  , _eChartSeries_splitLine :: Maybe JSVal
  , _eChartSeries_axisTick :: Maybe JSVal
  , _eChartSeries_axisLabel :: Maybe JSVal
  , _eChartSeries_pointer :: Maybe JSVal
  , _eChartSeries_details :: Maybe JSVal
  -- pictorialBar
  -- symbol stuff
  -- themeRiver
  -- custom
  -- other common options
  }
  deriving (Generic)

makeLenses ''EChartSeries

instance Default EChartSeries where

instance ToJSVal EChartSeries where
  toJSVal = toJSVal_generic (drop $ T.length "_eChartSeries_")

toEChartSeries :: Some SeriesT -> JSM EChartSeries
toEChartSeries (Some.Some st) =
  def
  -- common options
    { _eChartSeries_type = Just $ getSeriesType st
    , _eChartSeries_id = _series_id s
    , _eChartSeries_name = _series_name s
    }
  -- SeriesOptions
  & eChartSeries_coordinateSystem %%~ (const $ series_coordinateSystem_toJSVal st)
  >>= eChartSeries_xAxisIndex %%~ (const $ series_xAxisIndex_toJSVal st)
  >>= eChartSeries_yAxisIndex %%~ (const $ series_yAxisIndex_toJSVal st)
  >>= eChartSeries_polarIndex %%~ (const $ series_polarIndex_toJSVal st)
  >>= eChartSeries_symbol %%~ (const $ series_symbol_toJSVal st)
  >>= eChartSeries_symbolSize %%~ (const $ series_symbolSize_toJSVal st)
  >>= eChartSeries_symbolRotate %%~ (const $ series_symbolRotate_toJSVal st)
  >>= eChartSeries_symbolKeepAspect %%~ (const $ series_symbolKeepAspect_toJSVal st)
  >>= eChartSeries_symbolOffset %%~ (const $ series_symbolOffset_toJSVal st)
  >>= eChartSeries_showSymbol %%~ (const $ series_showSymbol_toJSVal st)
  >>= eChartSeries_showAllSymbol %%~ (const $ series_showAllSymbol_toJSVal st)
  >>= eChartSeries_hoverAnimation %%~ (const $ series_hoverAnimation_toJSVal st)
  >>= eChartSeries_legendHoverLink %%~ (const $ series_legendHoverLink_toJSVal st)
  >>= eChartSeries_stack %%~ (const $ series_stack_toJSVal st)
  >>= eChartSeries_cursor %%~ (const $ series_cursor_toJSVal st)
  >>= eChartSeries_connectNulls %%~ (const $ series_connectNulls_toJSVal st)
  >>= eChartSeries_clipOverflow %%~ (const $ series_clipOverflow_toJSVal st)
  >>= eChartSeries_step %%~ (const $ series_step_toJSVal st)
  >>= eChartSeries_label %%~ (const $ series_label_toJSVal st)
  >>= eChartSeries_itemStyle %%~ (const $ series_itemStyle_toJSVal st)
  >>= eChartSeries_lineStyle %%~ (const $ series_lineStyle_toJSVal st)
  >>= eChartSeries_areaStyle %%~ (const $ series_areaStyle_toJSVal st)
  -- >>= eChartSeries_emphasis %%~ (const $ series_emphasis_toJSVal st)
  >>= eChartSeries_smooth %%~ (const $ series_smooth_toJSVal st)
  >>= eChartSeries_smoothMonotone %%~ (const $ series_smoothMonotone_toJSVal st)
  >>= eChartSeries_sampling %%~ (const $ series_sampling_toJSVal st)
  >>= eChartSeries_dimensions %%~ (const $ series_dimensions_toJSVal st)
  >>= eChartSeries_encode %%~ (const $ series_encode_toJSVal st)
  >>= eChartSeries_seriesLayoutBy %%~ (const $ series_seriesLayoutBy_toJSVal st)
  >>= eChartSeries_datasetIndex %%~ (const $ series_datasetIndex_toJSVal st)
  >>= eChartSeries_data %%~ (const $ series_data_toJSVal st)
  >>= eChartSeries_markPoint %%~ (const $ series_markPoint_toJSVal st)
  >>= eChartSeries_markLine %%~ (const $ series_markLine_toJSVal st)
  >>= eChartSeries_markArea %%~ (const $ series_markArea_toJSVal st)
  >>= eChartSeries_zlevel %%~ (const $ series_zlevel_toJSVal st)
  >>= eChartSeries_z %%~ (const $ series_z_toJSVal st)
  >>= eChartSeries_animation %%~ (const $ series_animation_toJSVal st)
  >>= eChartSeries_animationThreshold %%~ (const $ series_animationThreshold_toJSVal st)
  >>= eChartSeries_animationDuration %%~ (const $ series_animationDuration_toJSVal st)
  >>= eChartSeries_animationEasing %%~ (const $ series_animationEasing_toJSVal st)
  >>= eChartSeries_animationDelay %%~ (const $ series_animationDelay_toJSVal st)
  >>= eChartSeries_animationDurationUpdate %%~ (const $ series_animationDurationUpdate_toJSVal st)
  >>= eChartSeries_animationEasingUpdate %%~ (const $ series_animationEasingUpdate_toJSVal st)
  >>= eChartSeries_animationDelayUpdate %%~ (const $ series_animationDelayUpdate_toJSVal st)
  >>= eChartSeries_tooltip %%~ (const $ series_tooltip_toJSVal st)
  >>= eChartSeries_barMinHeight %%~ (const $ series_barMinHeight_toJSVal st)
  >>= eChartSeries_barGap %%~ (const $ series_barGap_toJSVal st)
  >>= eChartSeries_barCategoryGap %%~ (const $ series_barCategoryGap_toJSVal st)
  >>= eChartSeries_large %%~ (const $ series_large_toJSVal st)
  >>= eChartSeries_largeThreshold %%~ (const $ series_largeThreshold_toJSVal st)
  >>= eChartSeries_progressive %%~ (const $ series_progressive_toJSVal st)
  >>= eChartSeries_progressiveThreshold %%~ (const $ series_progressiveThreshold_toJSVal st)
  >>= eChartSeries_progressiveChunkMode %%~ (const $ series_progressiveChunkMode_toJSVal st)
  >>= eChartSeries_hoverOffset %%~ (const $ series_hoverOffset_toJSVal st)
  >>= eChartSeries_selectedMode %%~ (const $ series_selectedMode_toJSVal st)
  >>= eChartSeries_selectedOffset %%~ (const $ series_selectedOffset_toJSVal st)
  >>= eChartSeries_clockwise %%~ (const $ series_clockwise_toJSVal st)
  >>= eChartSeries_minAngle %%~ (const $ series_minAngle_toJSVal st)
  >>= eChartSeries_roseType %%~ (const $ series_roseType_toJSVal st)
  >>= eChartSeries_avoidLabelOverlap %%~ (const $ series_avoidLabelOverlap_toJSVal st)
  >>= eChartSeries_stillShowZeroSum %%~ (const $ series_stillShowZeroSum_toJSVal st)
  >>= eChartSeries_center %%~ (const $ series_center_toJSVal st)
  >>= eChartSeries_radius %%~ (const $ series_radius_toJSVal st)
  >>= eChartSeries_geoIndex %%~ (const $ series_geoIndex_toJSVal st)
  >>= eChartSeries_calendarIndex %%~ (const $ series_calendarIndex_toJSVal st)
  >>= eChartSeries_showEffectOn %%~ (const $ series_showEffectOn_toJSVal st)
  >>= eChartSeries_rippleEffect %%~ (const $ series_rippleEffect_toJSVal st)
  >>= eChartSeries_radarIndex %%~ (const $ series_radarIndex_toJSVal st)
  >>= eChartSeries_left %%~ (const $ series_left_toJSVal st)
  >>= eChartSeries_top %%~ (const $ series_top_toJSVal st)
  >>= eChartSeries_right %%~ (const $ series_right_toJSVal st)
  >>= eChartSeries_bottom %%~ (const $ series_bottom_toJSVal st)
  >>= eChartSeries_width %%~ (const $ series_width_toJSVal st)
  >>= eChartSeries_height %%~ (const $ series_height_toJSVal st)
  >>= eChartSeries_layout %%~ (const $ series_layout_toJSVal st)
  >>= eChartSeries_orient %%~ (const $ series_orient_toJSVal st)
  >>= eChartSeries_roam %%~ (const $ series_roam_toJSVal st)
  >>= eChartSeries_expandAndCollapse %%~ (const $ series_expandAndCollapse_toJSVal st)
  >>= eChartSeries_initialTreeDepth %%~ (const $ series_initialTreeDepth_toJSVal st)
  >>= eChartSeries_leaves %%~ (const $ series_leaves_toJSVal st)
  >>= eChartSeries_squareRatio %%~ (const $ series_squareRatio_toJSVal st)
  >>= eChartSeries_leafDepth %%~ (const $ series_leafDepth_toJSVal st)
  >>= eChartSeries_drillDownIcon %%~ (const $ series_drillDownIcon_toJSVal st)
  >>= eChartSeries_nodeClick %%~ (const $ series_nodeClick_toJSVal st)
  >>= eChartSeries_zoomToNodeRatio %%~ (const $ series_zoomToNodeRatio_toJSVal st)
  >>= eChartSeries_levels %%~ (const $ series_levels_toJSVal st)
  >>= eChartSeries_silent %%~ (const $ series_silent_toJSVal st)
  >>= eChartSeries_visualDimension %%~ (const $ series_visualDimension_toJSVal st)
  >>= eChartSeries_visualMin %%~ (const $ series_visualMin_toJSVal st)
  >>= eChartSeries_visualMax %%~ (const $ series_visualMax_toJSVal st)
  >>= eChartSeries_colorAlpha %%~ (const $ series_colorAlpha_toJSVal st)
  >>= eChartSeries_colorSaturation %%~ (const $ series_colorSaturation_toJSVal st)
  >>= eChartSeries_colorMappingsBy %%~ (const $ series_colorMappingsBy_toJSVal st)
  >>= eChartSeries_visibleMin %%~ (const $ series_visibleMin_toJSVal st)
  >>= eChartSeries_childrenVisibleMin %%~ (const $ series_childrenVisibleMin_toJSVal st)
  >>= eChartSeries_breadcrumb %%~ (const $ series_breadcrumb_toJSVal st)
  >>= eChartSeries_sort %%~ (const $ series_sort_toJSVal st)
  >>= eChartSeries_renderLabelForZeroData %%~ (const $ series_renderLabelForZeroData_toJSVal st)
  >>= eChartSeries_downplay %%~ (const $ series_downplay_toJSVal st)
  >>= eChartSeries_boxWidth %%~ (const $ series_boxWidth_toJSVal st)
  >>= eChartSeries_barWidth %%~ (const $ series_barWidth_toJSVal st)
  >>= eChartSeries_barMinWidth %%~ (const $ series_barMinWidth_toJSVal st)
  >>= eChartSeries_barMaxWidth %%~ (const $ series_barMaxWidth_toJSVal st)
  >>= eChartSeries_blurSize %%~ (const $ series_blurSize_toJSVal st)
  >>= eChartSeries_minOpacity %%~ (const $ series_minOpacity_toJSVal st)
  >>= eChartSeries_maxOpacity %%~ (const $ series_maxOpacity_toJSVal st)
  >>= eChartSeries_aspectScale %%~ (const $ series_aspectScale_toJSVal st)
  >>= eChartSeries_boundingCoords %%~ (const $ series_boundingCoords_toJSVal st)
  >>= eChartSeries_zoom %%~ (const $ series_zoom_toJSVal st)
  >>= eChartSeries_scaleLimit %%~ (const $ series_scaleLimit_toJSVal st)
  >>= eChartSeries_nameMap %%~ (const $ series_nameMap_toJSVal st)
  >>= eChartSeries_layoutCenter %%~ (const $ series_layoutCenter_toJSVal st)
  >>= eChartSeries_mapValueCalculation %%~ (const $ series_mapValueCalculation_toJSVal st)
  >>= eChartSeries_showLegendSymbol %%~ (const $ series_showLegendSymbol_toJSVal st)
  >>= eChartSeries_parallelIndex %%~ (const $ series_parallelIndex_toJSVal st)
  >>= eChartSeries_inactiveOpacity %%~ (const $ series_inactiveOpacity_toJSVal st)
  >>= eChartSeries_activeOpacity %%~ (const $ series_activeOpacity_toJSVal st)
  >>= eChartSeries_realtime %%~ (const $ series_realtime_toJSVal st)
  >>= eChartSeries_polyline %%~ (const $ series_polyline_toJSVal st)
  >>= eChartSeries_circularRotateLayout %%~ (const $ series_circularRotateLayout_toJSVal st)
  >>= eChartSeries_force %%~ (const $ series_force_toJSVal st)
  >>= eChartSeries_nodeScaleRatio %%~ (const $ series_nodeScaleRatio_toJSVal st)
  >>= eChartSeries_draggable %%~ (const $ series_draggable_toJSVal st)
  >>= eChartSeries_edgeSymbol %%~ (const $ series_edgeSymbol_toJSVal st)
  >>= eChartSeries_edgeSymbolSize %%~ (const $ series_edgeSymbolSize_toJSVal st)
  >>= eChartSeries_categories %%~ (const $ series_categories_toJSVal st)
  >>= eChartSeries_links %%~ (const $ series_links_toJSVal st)
  >>= eChartSeries_nodeWidth %%~ (const $ series_nodeWidth_toJSVal st)
  >>= eChartSeries_nodeGap %%~ (const $ series_nodeGap_toJSVal st)
  >>= eChartSeries_layoutIterations %%~ (const $ series_layoutIterations_toJSVal st)
  >>= eChartSeries_focusNodeAdjacency %%~ (const $ series_focusNodeAdjacency_toJSVal st)
  >>= eChartSeries_min %%~ (const $ series_min_toJSVal st)
  >>= eChartSeries_max %%~ (const $ series_max_toJSVal st)
  >>= eChartSeries_minSize %%~ (const $ series_minSize_toJSVal st)
  >>= eChartSeries_maxSize %%~ (const $ series_maxSize_toJSVal st)
  >>= eChartSeries_gap %%~ (const $ series_gap_toJSVal st)
  >>= eChartSeries_funnelAlign %%~ (const $ series_funnelAlign_toJSVal st)
  >>= eChartSeries_labelLine %%~ (const $ series_labelLine_toJSVal st)
  >>= eChartSeries_startAngle %%~ (const $ series_startAngle_toJSVal st)
  >>= eChartSeries_endAngle %%~ (const $ series_endAngle_toJSVal st)
  >>= eChartSeries_splitNumber %%~ (const $ series_splitNumber_toJSVal st)
  >>= eChartSeries_axisLine %%~ (const $ series_axisLine_toJSVal st)
  >>= eChartSeries_splitLine %%~ (const $ series_splitLine_toJSVal st)
  >>= eChartSeries_axisTick %%~ (const $ series_axisTick_toJSVal st)
  >>= eChartSeries_axisLabel %%~ (const $ series_axisLabel_toJSVal st)
  >>= eChartSeries_pointer %%~ (const $ series_pointer_toJSVal st)
  >>= eChartSeries_details %%~ (const $ series_details_toJSVal st)
  where
    s = getSeries st

series_coordinateSystem_toJSVal :: SeriesT s -> JSM (Maybe JSVal)
series_coordinateSystem_toJSVal = \case
  (SeriesT_Line s) -> mapM toJSVal $ s ^? series_coordinateSystem . _Just
  _ -> return Nothing

series_xAxisIndex_toJSVal :: SeriesT s -> JSM (Maybe JSVal)
series_xAxisIndex_toJSVal = \case
  (SeriesT_Line s) -> mapM toJSVal $ s ^? series_xAxisIndex . _Just
  _ -> return Nothing

series_yAxisIndex_toJSVal :: SeriesT s -> JSM (Maybe JSVal)
series_yAxisIndex_toJSVal = \case
  (SeriesT_Line s) -> mapM toJSVal $ s ^? series_yAxisIndex . _Just
  _ -> return Nothing

series_polarIndex_toJSVal :: SeriesT s -> JSM (Maybe JSVal)
series_polarIndex_toJSVal = \case
  (SeriesT_Line s) -> mapM toJSVal $ s ^? series_polarIndex . _Just
  _ -> return Nothing

series_symbol_toJSVal :: SeriesT s -> JSM (Maybe JSVal)
series_symbol_toJSVal = \case
  (SeriesT_Line s) -> mapM toJSVal $ s ^? series_symbol . _Just
  _ -> return Nothing

series_symbolSize_toJSVal :: SeriesT s -> JSM (Maybe JSVal)
series_symbolSize_toJSVal = \case
  (SeriesT_Line s) -> mapM toJSVal $ s ^? series_symbolSize . _Just
  _ -> return Nothing

series_symbolRotate_toJSVal :: SeriesT s -> JSM (Maybe JSVal)
series_symbolRotate_toJSVal = \case
  (SeriesT_Line s) -> mapM toJSVal $ s ^? series_symbolRotate . _Just
  _ -> return Nothing

series_symbolKeepAspect_toJSVal :: SeriesT s -> JSM (Maybe JSVal)
series_symbolKeepAspect_toJSVal = \case
  (SeriesT_Line s) -> mapM toJSVal $ s ^? series_symbolKeepAspect . _Just
  _ -> return Nothing

series_symbolOffset_toJSVal :: SeriesT s -> JSM (Maybe JSVal)
series_symbolOffset_toJSVal = \case
  (SeriesT_Line s) -> mapM toJSVal $ s ^? series_symbolOffset . _Just
  _ -> return Nothing

series_showSymbol_toJSVal :: SeriesT s -> JSM (Maybe JSVal)
series_showSymbol_toJSVal = \case
  (SeriesT_Line s) -> mapM toJSVal $ s ^? series_showSymbol . _Just
  _ -> return Nothing

series_showAllSymbol_toJSVal :: SeriesT s -> JSM (Maybe JSVal)
series_showAllSymbol_toJSVal = \case
  (SeriesT_Line s) -> mapM toJSVal $ s ^? series_showAllSymbol . _Just
  _ -> return Nothing

series_hoverAnimation_toJSVal :: SeriesT s -> JSM (Maybe JSVal)
series_hoverAnimation_toJSVal = \case
  (SeriesT_Line s) -> mapM toJSVal $ s ^? series_hoverAnimation . _Just
  _ -> return Nothing

series_legendHoverLink_toJSVal :: SeriesT s -> JSM (Maybe JSVal)
series_legendHoverLink_toJSVal = \case
  (SeriesT_Line s) -> mapM toJSVal $ s ^? series_legendHoverLink . _Just
  _ -> return Nothing

series_stack_toJSVal :: SeriesT s -> JSM (Maybe JSVal)
series_stack_toJSVal = \case
  (SeriesT_Line s) -> mapM toJSVal $ s ^? series_stack . _Just
  _ -> return Nothing

series_cursor_toJSVal :: SeriesT s -> JSM (Maybe JSVal)
series_cursor_toJSVal = \case
  (SeriesT_Line s) -> mapM toJSVal $ s ^? series_cursor . _Just
  _ -> return Nothing

series_connectNulls_toJSVal :: SeriesT s -> JSM (Maybe JSVal)
series_connectNulls_toJSVal = \case
  (SeriesT_Line s) -> mapM toJSVal $ s ^? series_connectNulls . _Just
  _ -> return Nothing

series_clipOverflow_toJSVal :: SeriesT s -> JSM (Maybe JSVal)
series_clipOverflow_toJSVal = \case
  (SeriesT_Line s) -> mapM toJSVal $ s ^? series_clipOverflow . _Just
  _ -> return Nothing

series_step_toJSVal :: SeriesT s -> JSM (Maybe JSVal)
series_step_toJSVal = \case
  (SeriesT_Line s) -> mapM toJSVal $ s ^? series_step . _Just
  _ -> return Nothing

series_label_toJSVal :: SeriesT s -> JSM (Maybe JSVal)
series_label_toJSVal = \case
  (SeriesT_Line s) -> mapM toJSVal $ s ^? series_label . _Just
  _ -> return Nothing

series_itemStyle_toJSVal :: SeriesT s -> JSM (Maybe JSVal)
series_itemStyle_toJSVal = \case
  (SeriesT_Line s) -> mapM toJSVal $ s ^? series_itemStyle . _Just
  _ -> return Nothing

series_lineStyle_toJSVal :: SeriesT s -> JSM (Maybe JSVal)
series_lineStyle_toJSVal = \case
  (SeriesT_Line s) -> mapM toJSVal $ s ^? series_lineStyle . _Just
  _ -> return Nothing

series_areaStyle_toJSVal :: SeriesT s -> JSM (Maybe JSVal)
series_areaStyle_toJSVal = \case
  (SeriesT_Line s) -> mapM toJSVal $ s ^? series_areaStyle . _Just
  _ -> return Nothing

-- series_emphasis_toJSVal :: SeriesT s -> JSM (Maybe JSVal)
-- series_emphasis_toJSVal = \case
--   (SeriesT_Line s) -> mapM toJSVal $ s ^? series_emphasis . _Just
--   _ -> return Nothing

series_smooth_toJSVal :: SeriesT s -> JSM (Maybe JSVal)
series_smooth_toJSVal = \case
  (SeriesT_Line s) -> mapM eitherToJSVal $ s ^? series_smooth . _Just
  _ -> return Nothing

eitherToJSVal :: (ToJSVal a, ToJSVal b) => Either a b -> JSM JSVal
eitherToJSVal = \case
  (Left a) -> toJSVal a
  (Right b) -> toJSVal b

series_smoothMonotone_toJSVal :: SeriesT s -> JSM (Maybe JSVal)
series_smoothMonotone_toJSVal = \case
  (SeriesT_Line s) -> mapM toJSVal $ s ^? series_smoothMonotone . _Just
  _ -> return Nothing

series_sampling_toJSVal :: SeriesT s -> JSM (Maybe JSVal)
series_sampling_toJSVal = \case
  (SeriesT_Line s) -> mapM toJSVal $ s ^? series_sampling . _Just
  _ -> return Nothing

series_dimensions_toJSVal :: SeriesT s -> JSM (Maybe JSVal)
series_dimensions_toJSVal = \case
  -- (SeriesT_Line s) -> mapM toJSVal $ s ^? series_dimensions . _Just
  _ -> return Nothing

series_encode_toJSVal :: SeriesT s -> JSM (Maybe JSVal)
series_encode_toJSVal = \case
  -- (SeriesT_Line s) -> mapM toJSVal $ s ^? series_encode . _Just
  _ -> return Nothing

series_seriesLayoutBy_toJSVal :: SeriesT s -> JSM (Maybe JSVal)
series_seriesLayoutBy_toJSVal = \case
  (SeriesT_Line s) -> mapM toJSVal $ s ^? series_seriesLayoutBy . _Just
  _ -> return Nothing

series_datasetIndex_toJSVal :: SeriesT s -> JSM (Maybe JSVal)
series_datasetIndex_toJSVal = \case
  (SeriesT_Line s) -> mapM toJSVal $ s ^? series_datasetIndex . _Just
  _ -> return Nothing

series_data_toJSVal :: SeriesT s -> JSM (Maybe JSVal)
series_data_toJSVal = \case
  (SeriesT_Line s) -> mapM toJSVal $ s ^? series_data . _Just
  _ -> return Nothing

series_markPoint_toJSVal :: SeriesT s -> JSM (Maybe JSVal)
series_markPoint_toJSVal = \case
  (SeriesT_Line s) -> mapM toJSVal $ s ^? series_markPoint . _Just
  _ -> return Nothing

series_markLine_toJSVal :: SeriesT s -> JSM (Maybe JSVal)
series_markLine_toJSVal = \case
  (SeriesT_Line s) -> mapM toJSVal $ s ^? series_markLine . _Just
  _ -> return Nothing

series_markArea_toJSVal :: SeriesT s -> JSM (Maybe JSVal)
series_markArea_toJSVal = \case
  (SeriesT_Line s) -> mapM toJSVal $ s ^? series_markArea . _Just
  _ -> return Nothing

series_zlevel_toJSVal :: SeriesT s -> JSM (Maybe JSVal)
series_zlevel_toJSVal = \case
  (SeriesT_Line s) -> mapM toJSVal $ s ^? series_zlevel . _Just
  _ -> return Nothing

series_z_toJSVal :: SeriesT s -> JSM (Maybe JSVal)
series_z_toJSVal = \case
  (SeriesT_Line s) -> mapM toJSVal $ s ^? series_z . _Just
  _ -> return Nothing

series_animation_toJSVal :: SeriesT s -> JSM (Maybe JSVal)
series_animation_toJSVal = \case
  (SeriesT_Line s) -> mapM toJSVal $ s ^? series_animation . _Just
  _ -> return Nothing

series_animationThreshold_toJSVal :: SeriesT s -> JSM (Maybe JSVal)
series_animationThreshold_toJSVal = \case
  (SeriesT_Line s) -> mapM toJSVal $ s ^? series_animationThreshold . _Just
  _ -> return Nothing

series_animationDuration_toJSVal :: SeriesT s -> JSM (Maybe JSVal)
series_animationDuration_toJSVal = \case
  (SeriesT_Line s) -> mapM toJSVal $ s ^? series_animationDuration . _Just
  _ -> return Nothing

series_animationEasing_toJSVal :: SeriesT s -> JSM (Maybe JSVal)
series_animationEasing_toJSVal = \case
  (SeriesT_Line s) -> mapM toJSVal $ s ^? series_animationEasing . _Just
  _ -> return Nothing

series_animationDelay_toJSVal :: SeriesT s -> JSM (Maybe JSVal)
series_animationDelay_toJSVal = \case
  (SeriesT_Line s) -> mapM toJSVal $ s ^? series_animationDelay . _Just
  _ -> return Nothing

series_animationDurationUpdate_toJSVal :: SeriesT s -> JSM (Maybe JSVal)
series_animationDurationUpdate_toJSVal = \case
  (SeriesT_Line s) -> mapM toJSVal $ s ^? series_animationDurationUpdate . _Just
  _ -> return Nothing

series_animationEasingUpdate_toJSVal :: SeriesT s -> JSM (Maybe JSVal)
series_animationEasingUpdate_toJSVal = \case
  (SeriesT_Line s) -> mapM toJSVal $ s ^? series_animationEasingUpdate . _Just
  _ -> return Nothing

series_animationDelayUpdate_toJSVal :: SeriesT s -> JSM (Maybe JSVal)
series_animationDelayUpdate_toJSVal = \case
  (SeriesT_Line s) -> mapM toJSVal $ s ^? series_animationDelayUpdate . _Just
  _ -> return Nothing

series_tooltip_toJSVal :: SeriesT s -> JSM (Maybe JSVal)
series_tooltip_toJSVal = \case
  (SeriesT_Line s) -> mapM toJSVal $ s ^? series_tooltip . _Just
  _ -> return Nothing

series_barMinHeight_toJSVal :: SeriesT s -> JSM (Maybe JSVal)
series_barMinHeight_toJSVal = \case
  -- (SeriesT_Line s) -> mapM toJSVal $ s ^? series_barMinHeight . _Just
  _ -> return Nothing

series_barGap_toJSVal :: SeriesT s -> JSM (Maybe JSVal)
series_barGap_toJSVal = \case
  -- (SeriesT_Line s) -> mapM toJSVal $ s ^? series_barGap . _Just
  _ -> return Nothing

series_barCategoryGap_toJSVal :: SeriesT s -> JSM (Maybe JSVal)
series_barCategoryGap_toJSVal = \case
  -- (SeriesT_Line s) -> mapM toJSVal $ s ^? series_barCategoryGap . _Just
  _ -> return Nothing

series_large_toJSVal :: SeriesT s -> JSM (Maybe JSVal)
series_large_toJSVal = \case
  -- (SeriesT_Line s) -> mapM toJSVal $ s ^? series_large . _Just
  _ -> return Nothing

series_largeThreshold_toJSVal :: SeriesT s -> JSM (Maybe JSVal)
series_largeThreshold_toJSVal = \case
  -- (SeriesT_Line s) -> mapM toJSVal $ s ^? series_largeThreshold . _Just
  _ -> return Nothing

series_progressive_toJSVal :: SeriesT s -> JSM (Maybe JSVal)
series_progressive_toJSVal = \case
  -- (SeriesT_Line s) -> mapM toJSVal $ s ^? series_progressive . _Just
  _ -> return Nothing

series_progressiveThreshold_toJSVal :: SeriesT s -> JSM (Maybe JSVal)
series_progressiveThreshold_toJSVal = \case
  -- (SeriesT_Line s) -> mapM toJSVal $ s ^? series_progressiveThreshold . _Just
  _ -> return Nothing

series_progressiveChunkMode_toJSVal :: SeriesT s -> JSM (Maybe JSVal)
series_progressiveChunkMode_toJSVal = \case
  -- (SeriesT_Line s) -> mapM toJSVal $ s ^? series_progressiveChunkMode . _Just
  _ -> return Nothing

series_hoverOffset_toJSVal :: SeriesT s -> JSM (Maybe JSVal)
series_hoverOffset_toJSVal = \case
  -- (SeriesT_Line s) -> mapM toJSVal $ s ^? series_hoverOffset . _Just
  _ -> return Nothing

series_selectedMode_toJSVal :: SeriesT s -> JSM (Maybe JSVal)
series_selectedMode_toJSVal = \case
  -- (SeriesT_Line s) -> mapM toJSVal $ s ^? series_selectedMode . _Just
  _ -> return Nothing

series_selectedOffset_toJSVal :: SeriesT s -> JSM (Maybe JSVal)
series_selectedOffset_toJSVal = \case
  -- (SeriesT_Line s) -> mapM toJSVal $ s ^? series_selectedOffset . _Just
  _ -> return Nothing

series_clockwise_toJSVal :: SeriesT s -> JSM (Maybe JSVal)
series_clockwise_toJSVal = \case
  -- (SeriesT_Line s) -> mapM toJSVal $ s ^? series_clockwise . _Just
  _ -> return Nothing

series_minAngle_toJSVal :: SeriesT s -> JSM (Maybe JSVal)
series_minAngle_toJSVal = \case
  -- (SeriesT_Line s) -> mapM toJSVal $ s ^? series_minAngle . _Just
  _ -> return Nothing

series_roseType_toJSVal :: SeriesT s -> JSM (Maybe JSVal)
series_roseType_toJSVal = \case
  -- (SeriesT_Line s) -> mapM toJSVal $ s ^? series_roseType . _Just
  _ -> return Nothing

series_avoidLabelOverlap_toJSVal :: SeriesT s -> JSM (Maybe JSVal)
series_avoidLabelOverlap_toJSVal = \case
  -- (SeriesT_Line s) -> mapM toJSVal $ s ^? series_avoidLabelOverlap . _Just
  _ -> return Nothing

series_stillShowZeroSum_toJSVal :: SeriesT s -> JSM (Maybe JSVal)
series_stillShowZeroSum_toJSVal = \case
  -- (SeriesT_Line s) -> mapM toJSVal $ s ^? series_stillShowZeroSum . _Just
  _ -> return Nothing

series_center_toJSVal :: SeriesT s -> JSM (Maybe JSVal)
series_center_toJSVal = \case
  -- (SeriesT_Line s) -> mapM toJSVal $ s ^? series_center . _Just
  _ -> return Nothing

series_radius_toJSVal :: SeriesT s -> JSM (Maybe JSVal)
series_radius_toJSVal = \case
  -- (SeriesT_Line s) -> mapM toJSVal $ s ^? series_radius . _Just
  _ -> return Nothing

series_geoIndex_toJSVal :: SeriesT s -> JSM (Maybe JSVal)
series_geoIndex_toJSVal = \case
  -- (SeriesT_Line s) -> mapM toJSVal $ s ^? series_geoIndex . _Just
  _ -> return Nothing

series_calendarIndex_toJSVal :: SeriesT s -> JSM (Maybe JSVal)
series_calendarIndex_toJSVal = \case
  -- (SeriesT_Line s) -> mapM toJSVal $ s ^? series_calendarIndex . _Just
  _ -> return Nothing

series_showEffectOn_toJSVal :: SeriesT s -> JSM (Maybe JSVal)
series_showEffectOn_toJSVal = \case
  -- (SeriesT_Line s) -> mapM toJSVal $ s ^? series_showEffectOn . _Just
  _ -> return Nothing

series_rippleEffect_toJSVal :: SeriesT s -> JSM (Maybe JSVal)
series_rippleEffect_toJSVal = \case
  -- (SeriesT_Line s) -> mapM toJSVal $ s ^? series_rippleEffect . _Just
  _ -> return Nothing

series_radarIndex_toJSVal :: SeriesT s -> JSM (Maybe JSVal)
series_radarIndex_toJSVal = \case
  -- (SeriesT_Line s) -> mapM toJSVal $ s ^? series_radarIndex . _Just
  _ -> return Nothing

series_left_toJSVal :: SeriesT s -> JSM (Maybe JSVal)
series_left_toJSVal = \case
  -- (SeriesT_Line s) -> mapM toJSVal $ s ^? series_left . _Just
  _ -> return Nothing

series_top_toJSVal :: SeriesT s -> JSM (Maybe JSVal)
series_top_toJSVal = \case
  -- (SeriesT_Line s) -> mapM toJSVal $ s ^? series_top . _Just
  _ -> return Nothing

series_right_toJSVal :: SeriesT s -> JSM (Maybe JSVal)
series_right_toJSVal = \case
  -- (SeriesT_Line s) -> mapM toJSVal $ s ^? series_right . _Just
  _ -> return Nothing

series_bottom_toJSVal :: SeriesT s -> JSM (Maybe JSVal)
series_bottom_toJSVal = \case
  -- (SeriesT_Line s) -> mapM toJSVal $ s ^? series_bottom . _Just
  _ -> return Nothing

series_width_toJSVal :: SeriesT s -> JSM (Maybe JSVal)
series_width_toJSVal = \case
  -- (SeriesT_Line s) -> mapM toJSVal $ s ^? series_width . _Just
  _ -> return Nothing

series_height_toJSVal :: SeriesT s -> JSM (Maybe JSVal)
series_height_toJSVal = \case
  -- (SeriesT_Line s) -> mapM toJSVal $ s ^? series_height . _Just
  _ -> return Nothing

series_layout_toJSVal :: SeriesT s -> JSM (Maybe JSVal)
series_layout_toJSVal = \case
  -- (SeriesT_Line s) -> mapM toJSVal $ s ^? series_layout . _Just
  _ -> return Nothing

series_orient_toJSVal :: SeriesT s -> JSM (Maybe JSVal)
series_orient_toJSVal = \case
  -- (SeriesT_Line s) -> mapM toJSVal $ s ^? series_orient . _Just
  _ -> return Nothing

series_roam_toJSVal :: SeriesT s -> JSM (Maybe JSVal)
series_roam_toJSVal = \case
  -- (SeriesT_Line s) -> mapM toJSVal $ s ^? series_roam . _Just
  _ -> return Nothing

series_expandAndCollapse_toJSVal :: SeriesT s -> JSM (Maybe JSVal)
series_expandAndCollapse_toJSVal = \case
  -- (SeriesT_Line s) -> mapM toJSVal $ s ^? series_expandAndCollapse . _Just
  _ -> return Nothing

series_initialTreeDepth_toJSVal :: SeriesT s -> JSM (Maybe JSVal)
series_initialTreeDepth_toJSVal = \case
  -- (SeriesT_Line s) -> mapM toJSVal $ s ^? series_initialTreeDepth . _Just
  _ -> return Nothing

series_leaves_toJSVal :: SeriesT s -> JSM (Maybe JSVal)
series_leaves_toJSVal = \case
  -- (SeriesT_Line s) -> mapM toJSVal $ s ^? series_leaves . _Just
  _ -> return Nothing

series_squareRatio_toJSVal :: SeriesT s -> JSM (Maybe JSVal)
series_squareRatio_toJSVal = \case
  -- (SeriesT_Line s) -> mapM toJSVal $ s ^? series_squareRatio . _Just
  _ -> return Nothing

series_leafDepth_toJSVal :: SeriesT s -> JSM (Maybe JSVal)
series_leafDepth_toJSVal = \case
  -- (SeriesT_Line s) -> mapM toJSVal $ s ^? series_leafDepth . _Just
  _ -> return Nothing

series_drillDownIcon_toJSVal :: SeriesT s -> JSM (Maybe JSVal)
series_drillDownIcon_toJSVal = \case
  -- (SeriesT_Line s) -> mapM toJSVal $ s ^? series_drillDownIcon . _Just
  _ -> return Nothing

series_nodeClick_toJSVal :: SeriesT s -> JSM (Maybe JSVal)
series_nodeClick_toJSVal = \case
  -- (SeriesT_Line s) -> mapM toJSVal $ s ^? series_nodeClick . _Just
  _ -> return Nothing

series_zoomToNodeRatio_toJSVal :: SeriesT s -> JSM (Maybe JSVal)
series_zoomToNodeRatio_toJSVal = \case
  -- (SeriesT_Line s) -> mapM toJSVal $ s ^? series_zoomToNodeRatio . _Just
  _ -> return Nothing

series_levels_toJSVal :: SeriesT s -> JSM (Maybe JSVal)
series_levels_toJSVal = \case
  -- (SeriesT_Line s) -> mapM toJSVal $ s ^? series_levels . _Just
  _ -> return Nothing

series_silent_toJSVal :: SeriesT s -> JSM (Maybe JSVal)
series_silent_toJSVal = \case
  -- (SeriesT_Line s) -> mapM toJSVal $ s ^? series_silent . _Just
  _ -> return Nothing

series_visualDimension_toJSVal :: SeriesT s -> JSM (Maybe JSVal)
series_visualDimension_toJSVal = \case
  -- (SeriesT_Line s) -> mapM toJSVal $ s ^? series_visualDimension . _Just
  _ -> return Nothing

series_visualMin_toJSVal :: SeriesT s -> JSM (Maybe JSVal)
series_visualMin_toJSVal = \case
  -- (SeriesT_Line s) -> mapM toJSVal $ s ^? series_visualMin . _Just
  _ -> return Nothing

series_visualMax_toJSVal :: SeriesT s -> JSM (Maybe JSVal)
series_visualMax_toJSVal = \case
  -- (SeriesT_Line s) -> mapM toJSVal $ s ^? series_visualMax . _Just
  _ -> return Nothing

series_colorAlpha_toJSVal :: SeriesT s -> JSM (Maybe JSVal)
series_colorAlpha_toJSVal = \case
  -- (SeriesT_Line s) -> mapM toJSVal $ s ^? series_colorAlpha . _Just
  _ -> return Nothing

series_colorSaturation_toJSVal :: SeriesT s -> JSM (Maybe JSVal)
series_colorSaturation_toJSVal = \case
  -- (SeriesT_Line s) -> mapM toJSVal $ s ^? series_colorSaturation . _Just
  _ -> return Nothing

series_colorMappingsBy_toJSVal :: SeriesT s -> JSM (Maybe JSVal)
series_colorMappingsBy_toJSVal = \case
  -- (SeriesT_Line s) -> mapM toJSVal $ s ^? series_colorMappingsBy . _Just
  _ -> return Nothing

series_visibleMin_toJSVal :: SeriesT s -> JSM (Maybe JSVal)
series_visibleMin_toJSVal = \case
  -- (SeriesT_Line s) -> mapM toJSVal $ s ^? series_visibleMin . _Just
  _ -> return Nothing

series_childrenVisibleMin_toJSVal :: SeriesT s -> JSM (Maybe JSVal)
series_childrenVisibleMin_toJSVal = \case
  -- (SeriesT_Line s) -> mapM toJSVal $ s ^? series_childrenVisibleMin . _Just
  _ -> return Nothing

series_breadcrumb_toJSVal :: SeriesT s -> JSM (Maybe JSVal)
series_breadcrumb_toJSVal = \case
  -- (SeriesT_Line s) -> mapM toJSVal $ s ^? series_breadcrumb . _Just
  _ -> return Nothing

series_sort_toJSVal :: SeriesT s -> JSM (Maybe JSVal)
series_sort_toJSVal = \case
  -- (SeriesT_Line s) -> mapM toJSVal $ s ^? series_sort . _Just
  _ -> return Nothing

series_renderLabelForZeroData_toJSVal :: SeriesT s -> JSM (Maybe JSVal)
series_renderLabelForZeroData_toJSVal = \case
  -- (SeriesT_Line s) -> mapM toJSVal $ s ^? series_renderLabelForZeroData . _Just
  _ -> return Nothing

series_downplay_toJSVal :: SeriesT s -> JSM (Maybe JSVal)
series_downplay_toJSVal = \case
  -- (SeriesT_Line s) -> mapM toJSVal $ s ^? series_downplay . _Just
  _ -> return Nothing

series_boxWidth_toJSVal :: SeriesT s -> JSM (Maybe JSVal)
series_boxWidth_toJSVal = \case
  -- (SeriesT_Line s) -> mapM toJSVal $ s ^? series_boxWidth . _Just
  _ -> return Nothing

series_barWidth_toJSVal :: SeriesT s -> JSM (Maybe JSVal)
series_barWidth_toJSVal = \case
  -- (SeriesT_Line s) -> mapM toJSVal $ s ^? series_barWidth . _Just
  _ -> return Nothing

series_barMinWidth_toJSVal :: SeriesT s -> JSM (Maybe JSVal)
series_barMinWidth_toJSVal = \case
  -- (SeriesT_Line s) -> mapM toJSVal $ s ^? series_barMinWidth . _Just
  _ -> return Nothing

series_barMaxWidth_toJSVal :: SeriesT s -> JSM (Maybe JSVal)
series_barMaxWidth_toJSVal = \case
  -- (SeriesT_Line s) -> mapM toJSVal $ s ^? series_barMaxWidth . _Just
  _ -> return Nothing

series_blurSize_toJSVal :: SeriesT s -> JSM (Maybe JSVal)
series_blurSize_toJSVal = \case
  -- (SeriesT_Line s) -> mapM toJSVal $ s ^? series_blurSize . _Just
  _ -> return Nothing

series_minOpacity_toJSVal :: SeriesT s -> JSM (Maybe JSVal)
series_minOpacity_toJSVal = \case
  -- (SeriesT_Line s) -> mapM toJSVal $ s ^? series_minOpacity . _Just
  _ -> return Nothing

series_maxOpacity_toJSVal :: SeriesT s -> JSM (Maybe JSVal)
series_maxOpacity_toJSVal = \case
  -- (SeriesT_Line s) -> mapM toJSVal $ s ^? series_maxOpacity . _Just
  _ -> return Nothing

series_aspectScale_toJSVal :: SeriesT s -> JSM (Maybe JSVal)
series_aspectScale_toJSVal = \case
  -- (SeriesT_Line s) -> mapM toJSVal $ s ^? series_aspectScale . _Just
  _ -> return Nothing

series_boundingCoords_toJSVal :: SeriesT s -> JSM (Maybe JSVal)
series_boundingCoords_toJSVal = \case
  -- (SeriesT_Line s) -> mapM toJSVal $ s ^? series_boundingCoords . _Just
  _ -> return Nothing

series_zoom_toJSVal :: SeriesT s -> JSM (Maybe JSVal)
series_zoom_toJSVal = \case
  -- (SeriesT_Line s) -> mapM toJSVal $ s ^? series_zoom . _Just
  _ -> return Nothing

series_scaleLimit_toJSVal :: SeriesT s -> JSM (Maybe JSVal)
series_scaleLimit_toJSVal = \case
  -- (SeriesT_Line s) -> mapM toJSVal $ s ^? series_scaleLimit . _Just
  _ -> return Nothing

series_nameMap_toJSVal :: SeriesT s -> JSM (Maybe JSVal)
series_nameMap_toJSVal = \case
  -- (SeriesT_Line s) -> mapM toJSVal $ s ^? series_nameMap . _Just
  _ -> return Nothing

series_layoutCenter_toJSVal :: SeriesT s -> JSM (Maybe JSVal)
series_layoutCenter_toJSVal = \case
  -- (SeriesT_Line s) -> mapM toJSVal $ s ^? series_layoutCenter . _Just
  _ -> return Nothing

series_mapValueCalculation_toJSVal :: SeriesT s -> JSM (Maybe JSVal)
series_mapValueCalculation_toJSVal = \case
  -- (SeriesT_Line s) -> mapM toJSVal $ s ^? series_mapValueCalculation . _Just
  _ -> return Nothing

series_showLegendSymbol_toJSVal :: SeriesT s -> JSM (Maybe JSVal)
series_showLegendSymbol_toJSVal = \case
  -- (SeriesT_Line s) -> mapM toJSVal $ s ^? series_showLegendSymbol . _Just
  _ -> return Nothing

series_parallelIndex_toJSVal :: SeriesT s -> JSM (Maybe JSVal)
series_parallelIndex_toJSVal = \case
  -- (SeriesT_Line s) -> mapM toJSVal $ s ^? series_parallelIndex . _Just
  _ -> return Nothing

series_inactiveOpacity_toJSVal :: SeriesT s -> JSM (Maybe JSVal)
series_inactiveOpacity_toJSVal = \case
  -- (SeriesT_Line s) -> mapM toJSVal $ s ^? series_inactiveOpacity . _Just
  _ -> return Nothing

series_activeOpacity_toJSVal :: SeriesT s -> JSM (Maybe JSVal)
series_activeOpacity_toJSVal = \case
  -- (SeriesT_Line s) -> mapM toJSVal $ s ^? series_activeOpacity . _Just
  _ -> return Nothing

series_realtime_toJSVal :: SeriesT s -> JSM (Maybe JSVal)
series_realtime_toJSVal = \case
  -- (SeriesT_Line s) -> mapM toJSVal $ s ^? series_realtime . _Just
  _ -> return Nothing

series_polyline_toJSVal :: SeriesT s -> JSM (Maybe JSVal)
series_polyline_toJSVal = \case
  -- (SeriesT_Line s) -> mapM toJSVal $ s ^? series_polyline . _Just
  _ -> return Nothing

series_circularRotateLayout_toJSVal :: SeriesT s -> JSM (Maybe JSVal)
series_circularRotateLayout_toJSVal = \case
  -- (SeriesT_Line s) -> mapM toJSVal $ s ^? series_circularRotateLayout . _Just
  _ -> return Nothing

series_force_toJSVal :: SeriesT s -> JSM (Maybe JSVal)
series_force_toJSVal = \case
  -- (SeriesT_Line s) -> mapM toJSVal $ s ^? series_force . _Just
  _ -> return Nothing

series_nodeScaleRatio_toJSVal :: SeriesT s -> JSM (Maybe JSVal)
series_nodeScaleRatio_toJSVal = \case
  -- (SeriesT_Line s) -> mapM toJSVal $ s ^? series_nodeScaleRatio . _Just
  _ -> return Nothing

series_draggable_toJSVal :: SeriesT s -> JSM (Maybe JSVal)
series_draggable_toJSVal = \case
  -- (SeriesT_Line s) -> mapM toJSVal $ s ^? series_draggable . _Just
  _ -> return Nothing

series_edgeSymbol_toJSVal :: SeriesT s -> JSM (Maybe JSVal)
series_edgeSymbol_toJSVal = \case
  -- (SeriesT_Line s) -> mapM toJSVal $ s ^? series_edgeSymbol . _Just
  _ -> return Nothing

series_edgeSymbolSize_toJSVal :: SeriesT s -> JSM (Maybe JSVal)
series_edgeSymbolSize_toJSVal = \case
  -- (SeriesT_Line s) -> mapM toJSVal $ s ^? series_edgeSymbolSize . _Just
  _ -> return Nothing

series_categories_toJSVal :: SeriesT s -> JSM (Maybe JSVal)
series_categories_toJSVal = \case
  -- (SeriesT_Line s) -> mapM toJSVal $ s ^? series_categories . _Just
  _ -> return Nothing

series_links_toJSVal :: SeriesT s -> JSM (Maybe JSVal)
series_links_toJSVal = \case
  -- (SeriesT_Line s) -> mapM toJSVal $ s ^? series_links . _Just
  _ -> return Nothing

series_nodeWidth_toJSVal :: SeriesT s -> JSM (Maybe JSVal)
series_nodeWidth_toJSVal = \case
  -- (SeriesT_Line s) -> mapM toJSVal $ s ^? series_nodeWidth . _Just
  _ -> return Nothing

series_nodeGap_toJSVal :: SeriesT s -> JSM (Maybe JSVal)
series_nodeGap_toJSVal = \case
  -- (SeriesT_Line s) -> mapM toJSVal $ s ^? series_nodeGap . _Just
  _ -> return Nothing

series_layoutIterations_toJSVal :: SeriesT s -> JSM (Maybe JSVal)
series_layoutIterations_toJSVal = \case
  -- (SeriesT_Line s) -> mapM toJSVal $ s ^? series_layoutIterations . _Just
  _ -> return Nothing

series_focusNodeAdjacency_toJSVal :: SeriesT s -> JSM (Maybe JSVal)
series_focusNodeAdjacency_toJSVal = \case
  -- (SeriesT_Line s) -> mapM toJSVal $ s ^? series_focusNodeAdjacency . _Just
  _ -> return Nothing

series_min_toJSVal :: SeriesT s -> JSM (Maybe JSVal)
series_min_toJSVal = \case
  -- (SeriesT_Line s) -> mapM toJSVal $ s ^? series_min . _Just
  _ -> return Nothing

series_max_toJSVal :: SeriesT s -> JSM (Maybe JSVal)
series_max_toJSVal = \case
  -- (SeriesT_Line s) -> mapM toJSVal $ s ^? series_max . _Just
  _ -> return Nothing

series_minSize_toJSVal :: SeriesT s -> JSM (Maybe JSVal)
series_minSize_toJSVal = \case
  -- (SeriesT_Line s) -> mapM toJSVal $ s ^? series_minSize . _Just
  _ -> return Nothing

series_maxSize_toJSVal :: SeriesT s -> JSM (Maybe JSVal)
series_maxSize_toJSVal = \case
  -- (SeriesT_Line s) -> mapM toJSVal $ s ^? series_maxSize . _Just
  _ -> return Nothing

series_gap_toJSVal :: SeriesT s -> JSM (Maybe JSVal)
series_gap_toJSVal = \case
  -- (SeriesT_Line s) -> mapM toJSVal $ s ^? series_gap . _Just
  _ -> return Nothing

series_funnelAlign_toJSVal :: SeriesT s -> JSM (Maybe JSVal)
series_funnelAlign_toJSVal = \case
  -- (SeriesT_Line s) -> mapM toJSVal $ s ^? series_funnelAlign . _Just
  _ -> return Nothing

series_labelLine_toJSVal :: SeriesT s -> JSM (Maybe JSVal)
series_labelLine_toJSVal = \case
  -- (SeriesT_Line s) -> mapM toJSVal $ s ^? series_labelLine . _Just
  _ -> return Nothing

series_startAngle_toJSVal :: SeriesT s -> JSM (Maybe JSVal)
series_startAngle_toJSVal = \case
  -- (SeriesT_Line s) -> mapM toJSVal $ s ^? series_startAngle . _Just
  _ -> return Nothing

series_endAngle_toJSVal :: SeriesT s -> JSM (Maybe JSVal)
series_endAngle_toJSVal = \case
  -- (SeriesT_Line s) -> mapM toJSVal $ s ^? series_endAngle . _Just
  _ -> return Nothing

series_splitNumber_toJSVal :: SeriesT s -> JSM (Maybe JSVal)
series_splitNumber_toJSVal = \case
  -- (SeriesT_Line s) -> mapM toJSVal $ s ^? series_splitNumber . _Just
  _ -> return Nothing

series_axisLine_toJSVal :: SeriesT s -> JSM (Maybe JSVal)
series_axisLine_toJSVal = \case
  -- (SeriesT_Line s) -> mapM toJSVal $ s ^? series_axisLine . _Just
  _ -> return Nothing

series_splitLine_toJSVal :: SeriesT s -> JSM (Maybe JSVal)
series_splitLine_toJSVal = \case
  -- (SeriesT_Line s) -> mapM toJSVal $ s ^? series_splitLine . _Just
  _ -> return Nothing

series_axisTick_toJSVal :: SeriesT s -> JSM (Maybe JSVal)
series_axisTick_toJSVal = \case
  -- (SeriesT_Line s) -> mapM toJSVal $ s ^? series_axisTick . _Just
  _ -> return Nothing

series_axisLabel_toJSVal :: SeriesT s -> JSM (Maybe JSVal)
series_axisLabel_toJSVal = \case
  -- (SeriesT_Line s) -> mapM toJSVal $ s ^? series_axisLabel . _Just
  _ -> return Nothing

series_pointer_toJSVal :: SeriesT s -> JSM (Maybe JSVal)
series_pointer_toJSVal = \case
  -- (SeriesT_Line s) -> mapM toJSVal $ s ^? series_pointer . _Just
  _ -> return Nothing

series_details_toJSVal :: SeriesT s -> JSM (Maybe JSVal)
series_details_toJSVal = \case
  -- (SeriesT_Line s) -> mapM toJSVal $ s ^? series_details . _Just
  _ -> return Nothing

