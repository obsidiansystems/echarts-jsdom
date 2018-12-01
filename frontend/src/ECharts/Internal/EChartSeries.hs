{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
module ECharts.Internal.EChartSeries where

import Data.Aeson (ToJSON, genericToEncoding, genericToJSON, defaultOptions, Options(..))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Scientific
import Data.Time
import GHC.Generics (Generic)
import Data.Default (Default, def)
import qualified Data.Aeson as Aeson
import Data.Proxy
import Data.Some (Some)
import qualified Data.Some as Some
import Control.Lens

import ECharts.Types
import ECharts.Internal.EChartTypes
import ECharts.Series
import ECharts.Internal.EChartToolTip
import ECharts.ChartOptions

data EChartSeries = EChartSeries
  -- line options
  { _eChartSeries_type :: Maybe Text
  , _eChartSeries_id :: Maybe Text
  , _eChartSeries_name :: Maybe Text
  , _eChartSeries_coordinateSystem :: Maybe Aeson.Value
  , _eChartSeries_xAxisIndex :: Maybe Aeson.Value
  , _eChartSeries_yAxisIndex :: Maybe Aeson.Value
  , _eChartSeries_polarIndex :: Maybe Aeson.Value
  , _eChartSeries_symbol :: Maybe Aeson.Value
  , _eChartSeries_symbolSize :: Maybe Aeson.Value
  , _eChartSeries_symbolRotate :: Maybe Aeson.Value
  , _eChartSeries_symbolKeepAspect :: Maybe Aeson.Value
  , _eChartSeries_symbolOffset :: Maybe Aeson.Value
  , _eChartSeries_showSymbol :: Maybe Aeson.Value
  , _eChartSeries_showAllSymbol :: Maybe Aeson.Value
  , _eChartSeries_hoverAnimation :: Maybe Aeson.Value
  , _eChartSeries_legendHoverLink :: Maybe Aeson.Value
  , _eChartSeries_stack :: Maybe Aeson.Value
  , _eChartSeries_cursor :: Maybe Aeson.Value
  , _eChartSeries_connectNulls :: Maybe Aeson.Value
  , _eChartSeries_clipOverflow :: Maybe Aeson.Value
  , _eChartSeries_step :: Maybe Aeson.Value
  , _eChartSeries_label :: Maybe Aeson.Value
  , _eChartSeries_itemStyle :: Maybe Aeson.Value
  , _eChartSeries_lineStyle :: Maybe Aeson.Value
  , _eChartSeries_areaStyle :: Maybe Aeson.Value
  , _eChartSeries_emphasis :: Maybe Aeson.Value
  , _eChartSeries_smooth :: Maybe Aeson.Value
  , _eChartSeries_smoothMonotone :: Maybe Aeson.Value
  , _eChartSeries_sampling :: Maybe Aeson.Value
  , _eChartSeries_dimensions :: Maybe Aeson.Value
  , _eChartSeries_encode :: Maybe Aeson.Value
  , _eChartSeries_seriesLayoutBy :: Maybe Aeson.Value
  , _eChartSeries_datasetIndex :: Maybe Aeson.Value
  , _eChartSeries_data :: Maybe Aeson.Value
  , _eChartSeries_markPoint :: Maybe Aeson.Value
  , _eChartSeries_markLine :: Maybe Aeson.Value
  , _eChartSeries_markArea :: Maybe Aeson.Value
  , _eChartSeries_zlevel :: Maybe Aeson.Value
  , _eChartSeries_z :: Maybe Aeson.Value
  , _eChartSeries_animation :: Maybe Aeson.Value
  , _eChartSeries_animationOptions :: Maybe Aeson.Value
  , _eChartSeries_tooltip :: Maybe Aeson.Value
  -- bar options
  -- candlestick has AbsOrPercent
  -- , _eChartSeries_barWidth :: Maybe Aeson.Value
  -- , _eChartSeries_barMaxWidth :: Maybe Aeson.Value
  , _eChartSeries_barMinHeight :: Maybe Aeson.Value
  , _eChartSeries_barGap :: Maybe Aeson.Value
  , _eChartSeries_barCategoryGap :: Maybe Aeson.Value
  , _eChartSeries_large :: Maybe Aeson.Value
  , _eChartSeries_largeThreshold :: Maybe Aeson.Value
  , _eChartSeries_progressive :: Maybe Aeson.Value
  , _eChartSeries_progressiveThreshold :: Maybe Aeson.Value
  , _eChartSeries_progressiveChunkMode :: Maybe Aeson.Value
  -- pie options
  , _eChartSeries_hoverOffset :: Maybe Aeson.Value
  , _eChartSeries_selectedMode :: Maybe Aeson.Value
  , _eChartSeries_selectedOffset :: Maybe Aeson.Value
  , _eChartSeries_clockwise :: Maybe Aeson.Value
  , _eChartSeries_minAngle :: Maybe Aeson.Value
  , _eChartSeries_roseType :: Maybe Aeson.Value
  , _eChartSeries_avoidLabelOverlap :: Maybe Aeson.Value
  , _eChartSeries_stillShowZeroSum :: Maybe Aeson.Value
  , _eChartSeries_center :: Maybe Aeson.Value
  , _eChartSeries_radius :: Maybe Aeson.Value
  -- scatter options
  , _eChartSeries_geoIndex :: Maybe Aeson.Value
  , _eChartSeries_calendarIndex :: Maybe Aeson.Value
  -- effectScatter options
  , _eChartSeries_showEffectOn :: Maybe Aeson.Value
  , _eChartSeries_rippleEffect :: Maybe Aeson.Value
  -- radar options
  , _eChartSeries_radarIndex :: Maybe Aeson.Value
  -- tree options
  , _eChartSeries_left :: Maybe Aeson.Value
  , _eChartSeries_top :: Maybe Aeson.Value
  , _eChartSeries_right :: Maybe Aeson.Value
  , _eChartSeries_bottom :: Maybe Aeson.Value
  , _eChartSeries_width :: Maybe Aeson.Value
  , _eChartSeries_height :: Maybe Aeson.Value
  , _eChartSeries_layout :: Maybe Aeson.Value
  , _eChartSeries_orient :: Maybe Aeson.Value
  , _eChartSeries_roam :: Maybe Aeson.Value
  , _eChartSeries_expandAndCollapse :: Maybe Aeson.Value
  , _eChartSeries_initialTreeDepth :: Maybe Aeson.Value
  , _eChartSeries_leaves :: Maybe Aeson.Value
  -- treemap options
  , _eChartSeries_squareRatio :: Maybe Aeson.Value
  , _eChartSeries_leafDepth :: Maybe Aeson.Value
  , _eChartSeries_drillDownIcon :: Maybe Aeson.Value
  , _eChartSeries_nodeClick :: Maybe Aeson.Value
  , _eChartSeries_zoomToNodeRatio :: Maybe Aeson.Value
  , _eChartSeries_levels :: Maybe Aeson.Value
  , _eChartSeries_silent :: Maybe Aeson.Value
  , _eChartSeries_visualDimension :: Maybe Aeson.Value
  , _eChartSeries_visualMin :: Maybe Aeson.Value
  , _eChartSeries_visualMax :: Maybe Aeson.Value
  , _eChartSeries_colorAlpha :: Maybe Aeson.Value
  , _eChartSeries_colorSaturation :: Maybe Aeson.Value
  , _eChartSeries_colorMappingsBy :: Maybe Aeson.Value
  , _eChartSeries_visibleMin :: Maybe Aeson.Value
  , _eChartSeries_childrenVisibleMin :: Maybe Aeson.Value
  , _eChartSeries_breadcrumb :: Maybe Aeson.Value
  -- sunburst options
  , _eChartSeries_sort :: Maybe Aeson.Value
  , _eChartSeries_renderLabelForZeroData :: Maybe Aeson.Value
  , _eChartSeries_downplay :: Maybe Aeson.Value
  -- boxplot options
  , _eChartSeries_boxWidth :: Maybe Aeson.Value
  -- candlestick options
  , _eChartSeries_barWidth :: Maybe Aeson.Value
  , _eChartSeries_barMinWidth :: Maybe Aeson.Value
  , _eChartSeries_barMaxWidth :: Maybe Aeson.Value
  -- heatmap
  , _eChartSeries_blurSize :: Maybe Aeson.Value
  , _eChartSeries_minOpacity :: Maybe Aeson.Value
  , _eChartSeries_maxOpacity :: Maybe Aeson.Value
  -- map
  , _eChartSeries_aspectScale :: Maybe Aeson.Value
  , _eChartSeries_boundingCoords :: Maybe Aeson.Value
  , _eChartSeries_zoom :: Maybe Aeson.Value
  , _eChartSeries_scaleLimit :: Maybe Aeson.Value
  , _eChartSeries_nameMap :: Maybe Aeson.Value
  , _eChartSeries_layoutCenter :: Maybe Aeson.Value
  , _eChartSeries_mapValueCalculation :: Maybe Aeson.Value
  , _eChartSeries_showLegendSymbol :: Maybe Aeson.Value
  -- parallel
  , _eChartSeries_parallelIndex :: Maybe Aeson.Value
  , _eChartSeries_inactiveOpacity :: Maybe Aeson.Value
  , _eChartSeries_activeOpacity :: Maybe Aeson.Value
  , _eChartSeries_realtime :: Maybe Aeson.Value
  -- lines
  , _eChartSeries_polyline :: Maybe Aeson.Value
  -- graph
  , _eChartSeries_circularRotateLayout :: Maybe Aeson.Value
  , _eChartSeries_force :: Maybe Aeson.Value
  , _eChartSeries_nodeScaleRatio :: Maybe Aeson.Value
  , _eChartSeries_draggable :: Maybe Aeson.Value
  -- focusNodeAdjacency :: Maybe Aeson.Value
  , _eChartSeries_edgeSymbol :: Maybe Aeson.Value
  , _eChartSeries_edgeSymbolSize :: Maybe Aeson.Value
  , _eChartSeries_categories :: Maybe Aeson.Value
  , _eChartSeries_links :: Maybe Aeson.Value
  -- sankey
  , _eChartSeries_nodeWidth :: Maybe Aeson.Value
  , _eChartSeries_nodeGap :: Maybe Aeson.Value
  , _eChartSeries_layoutIterations :: Maybe Aeson.Value
  , _eChartSeries_focusNodeAdjacency :: Maybe Aeson.Value
  -- funnel
  , _eChartSeries_min :: Maybe Aeson.Value
  , _eChartSeries_max :: Maybe Aeson.Value
  , _eChartSeries_minSize :: Maybe Aeson.Value
  , _eChartSeries_maxSize :: Maybe Aeson.Value
  , _eChartSeries_gap :: Maybe Aeson.Value
  , _eChartSeries_funnelAlign :: Maybe Aeson.Value
  , _eChartSeries_labelLine :: Maybe Aeson.Value
  -- gauge
  , _eChartSeries_startAngle :: Maybe Aeson.Value
  , _eChartSeries_endAngle :: Maybe Aeson.Value
  , _eChartSeries_splitNumber :: Maybe Aeson.Value
  , _eChartSeries_axisLine :: Maybe Aeson.Value
  , _eChartSeries_splitLine :: Maybe Aeson.Value
  , _eChartSeries_axisTick :: Maybe Aeson.Value
  , _eChartSeries_axisLabel :: Maybe Aeson.Value
  , _eChartSeries_pointer :: Maybe Aeson.Value
  , _eChartSeries_details :: Maybe Aeson.Value
  -- pictorialBar
  -- symbol stuff
  -- themeRiver
  -- custom
  -- other common options
  }
  deriving (Generic)

instance Default EChartSeries where

instance ToJSON EChartSeries where
  toJSON = genericToJSON $ defaultOptions
    { fieldLabelModifier = drop $ T.length "_eChartSeries_"
    , omitNothingFields = True
    }
  toEncoding = genericToEncoding $ defaultOptions
    { fieldLabelModifier = drop $ T.length "_eChartSeries_"
    , omitNothingFields = True
    }

toEChartSeries :: Some SeriesT -> EChartSeries
toEChartSeries (Some.This st) = def
  -- common options
  { _eChartSeries_type = Just $ getSeriesType st
  , _eChartSeries_id = _series_id s
  , _eChartSeries_name = _series_name s
  -- SeriesOptions
  , _eChartSeries_data                   = series_data_toJson st
  , _eChartSeries_coordinateSystem       = series_coordinateSystem_toJson st
  , _eChartSeries_xAxisIndex             = series_xAxisIndex_toJson st
  , _eChartSeries_yAxisIndex             = series_yAxisIndex_toJson st
  , _eChartSeries_polarIndex             = series_polarIndex_toJson st
  , _eChartSeries_symbol                 = series_symbol_toJson st
  , _eChartSeries_symbolSize             = series_symbolSize_toJson st
  , _eChartSeries_symbolRotate           = series_symbolRotate_toJson st
  , _eChartSeries_symbolKeepAspect       = series_symbolKeepAspect_toJson st
  , _eChartSeries_symbolOffset           = series_symbolOffset_toJson st
  , _eChartSeries_showSymbol             = series_showSymbol_toJson st
  , _eChartSeries_showAllSymbol          = series_showAllSymbol_toJson st
  , _eChartSeries_hoverAnimation         = series_hoverAnimation_toJson st
  , _eChartSeries_legendHoverLink        = series_legendHoverLink_toJson st
  , _eChartSeries_stack                  = series_stack_toJson st
  , _eChartSeries_cursor                 = series_cursor_toJson st
  , _eChartSeries_connectNulls           = series_connectNulls_toJson st
  , _eChartSeries_clipOverflow           = series_clipOverflow_toJson st
  , _eChartSeries_step                   = series_step_toJson st
  , _eChartSeries_label                  = series_label_toJson st
  , _eChartSeries_itemStyle              = series_itemStyle_toJson st
  , _eChartSeries_lineStyle              = series_lineStyle_toJson st
  , _eChartSeries_areaStyle              = series_areaStyle_toJson st
  -- , _eChartSeries_emphasis               = series_emphasis_toJson st
  , _eChartSeries_smooth                 = series_smooth_toJson st
  , _eChartSeries_smoothMonotone         = series_smoothMonotone_toJson st
  , _eChartSeries_sampling               = series_sampling_toJson st
  -- , _eChartSeries_dimensions             = series_dimensions_toJson st
  -- , _eChartSeries_encode                 = series_encode_toJson st
  , _eChartSeries_seriesLayoutBy         = series_seriesLayoutBy_toJson st
  , _eChartSeries_datasetIndex           = series_datasetIndex_toJson st
  , _eChartSeries_markPoint              = series_markPoint_toJson st
  , _eChartSeries_markLine               = series_markLine_toJson st
  , _eChartSeries_markArea               = series_markArea_toJson st
  , _eChartSeries_zlevel                 = series_zlevel_toJson st
  , _eChartSeries_z                      = series_z_toJson st
  , _eChartSeries_silent                 = series_silent_toJson st
  , _eChartSeries_animation              = series_animation_toJson st
  -- , _eChartSeries_animationOptions       = series_animationOptions_toJson st
  , _eChartSeries_tooltip                = series_tooltip_toJson st
  -- , _eChartSeries_barMinHeight           = series_barMinHeight_toJson st
  -- , _eChartSeries_barGap                 = series_barGap_toJson st
  -- , _eChartSeries_barCategoryGap         = series_barCategoryGap_toJson st
  -- , _eChartSeries_large                  = series_large_toJson st
  -- , _eChartSeries_largeThreshold         = series_largeThreshold_toJson st
  -- , _eChartSeries_progressive            = series_progressive_toJson st
  -- , _eChartSeries_progressiveThreshold   = series_progressiveThreshold_toJson st
  -- , _eChartSeries_progressiveChunkMode   = series_progressiveChunkMode_toJson st
  -- , _eChartSeries_hoverOffset            = series_hoverOffset_toJson st
  -- , _eChartSeries_selectedMode           = series_selectedMode_toJson st
  -- , _eChartSeries_selectedOffset         = series_selectedOffset_toJson st
  -- , _eChartSeries_clockwise              = series_clockwise_toJson st
  -- , _eChartSeries_minAngle               = series_minAngle_toJson st
  -- , _eChartSeries_roseType               = series_roseType_toJson st
  -- , _eChartSeries_avoidLabelOverlap      = series_avoidLabelOverlap_toJson st
  -- , _eChartSeries_stillShowZeroSum       = series_stillShowZeroSum_toJson st
  -- , _eChartSeries_center                 = series_center_toJson st
  -- , _eChartSeries_radius                 = series_radius_toJson st
  -- , _eChartSeries_geoIndex               = series_geoIndex_toJson st
  -- , _eChartSeries_calendarIndex          = series_calendarIndex_toJson st
  -- , _eChartSeries_showEffectOn           = series_showEffectOn_toJson st
  -- , _eChartSeries_rippleEffect           = series_rippleEffect_toJson st
  -- , _eChartSeries_radarIndex             = series_radarIndex_toJson st
  -- , _eChartSeries_left                   = series_left_toJson st
  -- , _eChartSeries_top                    = series_top_toJson st
  -- , _eChartSeries_right                  = series_right_toJson st
  -- , _eChartSeries_bottom                 = series_bottom_toJson st
  -- , _eChartSeries_width                  = series_width_toJson st
  -- , _eChartSeries_height                 = series_height_toJson st
  -- , _eChartSeries_layout                 = series_layout_toJson st
  -- , _eChartSeries_orient                 = series_orient_toJson st
  -- , _eChartSeries_roam                   = series_roam_toJson st
  -- , _eChartSeries_expandAndCollapse      = series_expandAndCollapse_toJson st
  -- , _eChartSeries_initialTreeDepth       = series_initialTreeDepth_toJson st
  -- , _eChartSeries_leaves                 = series_leaves_toJson st
  -- , _eChartSeries_squareRatio            = series_squareRatio_toJson st
  -- , _eChartSeries_leafDepth              = series_leafDepth_toJson st
  -- , _eChartSeries_drillDownIcon          = series_drillDownIcon_toJson st
  -- , _eChartSeries_nodeClick              = series_nodeClick_toJson st
  -- , _eChartSeries_zoomToNodeRatio        = series_zoomToNodeRatio_toJson st
  -- , _eChartSeries_levels                 = series_levels_toJson st
  -- , _eChartSeries_visualDimension        = _series_visualDimension        s
  -- , _eChartSeries_visualMin              = _series_visualMin              s
  -- , _eChartSeries_visualMax              = _series_visualMax              s
  -- , _eChartSeries_colorAlpha             = _series_colorAlpha             s
  -- , _eChartSeries_colorSaturation        = _series_colorSaturation        s
  -- , _eChartSeries_colorMappingsBy        = _series_colorMappingsBy        s
  -- , _eChartSeries_visibleMin             = _series_visibleMin             s
  -- , _eChartSeries_childrenVisibleMin     = _series_childrenVisibleMin     s
  -- , _eChartSeries_breadcrumb             = _series_breadcrumb             s
  -- , _eChartSeries_sort                   = _series_sort                   s
  -- , _eChartSeries_renderLabelForZeroData = _series_renderLabelForZeroData s
  -- , _eChartSeries_downplay               = _series_downplay               s
  -- , _eChartSeries_boxWidth               = _series_boxWidth               s
  -- , _eChartSeries_barWidth               = _series_barWidth               s
  -- , _eChartSeries_barMinWidth            = _series_barMinWidth            s
  -- , _eChartSeries_barMaxWidth            = _series_barMaxWidth            s
  -- , _eChartSeries_blurSize               = _series_blurSize               s
  -- , _eChartSeries_minOpacity             = _series_minOpacity             s
  -- , _eChartSeries_maxOpacity             = _series_maxOpacity             s
  -- , _eChartSeries_aspectScale            = _series_aspectScale            s
  -- , _eChartSeries_boundingCoords         = _series_boundingCoords         s
  -- , _eChartSeries_zoom                   = _series_zoom                   s
  -- , _eChartSeries_scaleLimit             = _series_scaleLimit             s
  -- , _eChartSeries_nameMap                = _series_nameMap                s
  -- , _eChartSeries_layoutCenter           = _series_layoutCenter           s
  -- , _eChartSeries_mapValueCalculation    = _series_mapValueCalculation    s
  -- , _eChartSeries_showLegendSymbol       = _series_showLegendSymbol       s
  -- , _eChartSeries_parallelIndex          = _series_parallelIndex          s
  -- , _eChartSeries_inactiveOpacity        = _series_inactiveOpacity        s
  -- , _eChartSeries_activeOpacity          = _series_activeOpacity          s
  -- , _eChartSeries_realtime               = _series_realtime               s
  -- , _eChartSeries_polyline               = _series_polyline               s
  -- , _eChartSeries_circularRotateLayout   = _series_circularRotateLayout   s
  -- , _eChartSeries_force                  = _series_force                  s
  -- , _eChartSeries_nodeScaleRatio         = _series_nodeScaleRatio         s
  -- , _eChartSeries_draggable              = _series_draggable              s
  -- , _eChartSeries_edgeSymbol             = _series_edgeSymbol             s
  -- , _eChartSeries_edgeSymbolSize         = _series_edgeSymbolSize         s
  -- , _eChartSeries_categories             = _series_categories             s
  -- , _eChartSeries_links                  = _series_links                  s
  -- , _eChartSeries_nodeWidth              = _series_nodeWidth              s
  -- , _eChartSeries_nodeGap                = _series_nodeGap                s
  -- , _eChartSeries_layoutIterations       = _series_layoutIterations       s
  -- , _eChartSeries_focusNodeAdjacency     = _series_focusNodeAdjacency     s
  -- , _eChartSeries_min                    = _series_min                    s
  -- , _eChartSeries_max                    = _series_max                    s
  -- , _eChartSeries_minSize                = _series_minSize                s
  -- , _eChartSeries_maxSize                = _series_maxSize                s
  -- , _eChartSeries_gap                    = _series_gap                    s
  -- , _eChartSeries_funnelAlign            = _series_funnelAlign            s
  -- , _eChartSeries_labelLine              = _series_labelLine              s
  -- , _eChartSeries_startAngle             = _series_startAngle             s
  -- , _eChartSeries_endAngle               = _series_endAngle               s
  -- , _eChartSeries_splitNumber            = _series_splitNumber            s
  -- , _eChartSeries_axisLine               = _series_axisLine               s
  -- , _eChartSeries_splitLine              = _series_splitLine              s
  -- , _eChartSeries_axisTick               = _series_axisTick               s
  -- , _eChartSeries_axisLabel              = _series_axisLabel              s
  -- , _eChartSeries_pointer                = _series_pointer                s
  -- , _eChartSeries_details                = _series_details                s
  }
  where
    s = getSeries st

series_data_toJson :: SeriesT s -> Maybe Aeson.Value
series_data_toJson = \case
  (SeriesT_Line s) -> s ^? series_data . _Just . to Aeson.toJSON
  (SeriesT_Pie s)  -> s ^? series_data . _Just . to Aeson.toJSON
  _ -> Nothing

series_coordinateSystem_toJson :: SeriesT s -> Maybe Aeson.Value
series_coordinateSystem_toJson = \case
  (SeriesT_Line s) -> s ^? series_coordinateSystem . _Just . to Aeson.toJSON
  _ -> Nothing

series_polarIndex_toJson :: SeriesT s -> Maybe Aeson.Value
series_polarIndex_toJson = \case
  (SeriesT_Line s) -> s ^? series_polarIndex . _Just . to Aeson.toJSON
  _ -> Nothing

series_symbolRotate_toJson :: SeriesT s -> Maybe Aeson.Value
series_symbolRotate_toJson = \case
  (SeriesT_Line s) -> s ^? series_symbolRotate . _Just . to Aeson.toJSON
  _ -> Nothing

series_symbolKeepAspect_toJson :: SeriesT s -> Maybe Aeson.Value
series_symbolKeepAspect_toJson = \case
  (SeriesT_Line s) -> s ^? series_symbolKeepAspect . _Just . to Aeson.toJSON
  _ -> Nothing

series_symbolOffset_toJson :: SeriesT s -> Maybe Aeson.Value
series_symbolOffset_toJson = \case
  (SeriesT_Line s) -> s ^? series_symbolOffset . _Just . to Aeson.toJSON
  _ -> Nothing

series_showAllSymbol_toJson :: SeriesT s -> Maybe Aeson.Value
series_showAllSymbol_toJson = \case
  (SeriesT_Line s) -> s ^? series_showAllSymbol . _Just . to Aeson.toJSON
  _ -> Nothing

series_legendHoverLink_toJson :: SeriesT s -> Maybe Aeson.Value
series_legendHoverLink_toJson = \case
  (SeriesT_Line s) -> s ^? series_legendHoverLink . _Just . to Aeson.toJSON
  _ -> Nothing

series_cursor_toJson :: SeriesT s -> Maybe Aeson.Value
series_cursor_toJson = \case
  (SeriesT_Line s) -> s ^? series_cursor . _Just . to Aeson.toJSON
  _ -> Nothing

series_connectNulls_toJson :: SeriesT s -> Maybe Aeson.Value
series_connectNulls_toJson = \case
  (SeriesT_Line s) -> s ^? series_connectNulls . _Just . to Aeson.toJSON
  _ -> Nothing

series_clipOverflow_toJson :: SeriesT s -> Maybe Aeson.Value
series_clipOverflow_toJson = \case
  (SeriesT_Line s) -> s ^? series_clipOverflow . _Just . to Aeson.toJSON
  _ -> Nothing

series_step_toJson :: SeriesT s -> Maybe Aeson.Value
series_step_toJson = \case
  (SeriesT_Line s) -> s ^? series_step . _Just . to Aeson.toJSON
  _ -> Nothing

series_smoothMonotone_toJson :: SeriesT s -> Maybe Aeson.Value
series_smoothMonotone_toJson = \case
  (SeriesT_Line s) -> s ^? series_smoothMonotone . _Just . to Aeson.toJSON
  _ -> Nothing

series_sampling_toJson :: SeriesT s -> Maybe Aeson.Value
series_sampling_toJson = \case
  (SeriesT_Line s) -> s ^? series_sampling . _Just . to Aeson.toJSON
  _ -> Nothing

series_seriesLayoutBy_toJson :: SeriesT s -> Maybe Aeson.Value
series_seriesLayoutBy_toJson = \case
  (SeriesT_Line s) -> s ^? series_seriesLayoutBy . _Just . to Aeson.toJSON
  _ -> Nothing

series_datasetIndex_toJson :: SeriesT s -> Maybe Aeson.Value
series_datasetIndex_toJson = \case
  (SeriesT_Line s) -> s ^? series_datasetIndex . _Just . to Aeson.toJSON
  _ -> Nothing

series_markPoint_toJson :: SeriesT s -> Maybe Aeson.Value
series_markPoint_toJson = \case
  (SeriesT_Line s) -> s ^? series_markPoint . _Just . to Aeson.toJSON
  _ -> Nothing

series_zlevel_toJson :: SeriesT s -> Maybe Aeson.Value
series_zlevel_toJson = \case
  (SeriesT_Line s) -> s ^? series_zlevel . _Just . to Aeson.toJSON
  _ -> Nothing

series_z_toJson :: SeriesT s -> Maybe Aeson.Value
series_z_toJson = \case
  (SeriesT_Line s) -> s ^? series_z . _Just . to Aeson.toJSON
  _ -> Nothing

series_silent_toJson :: SeriesT s -> Maybe Aeson.Value
series_silent_toJson = \case
  (SeriesT_Line s) -> s ^? series_silent . _Just . to Aeson.toJSON
  _ -> Nothing

series_tooltip_toJson :: SeriesT s -> Maybe Aeson.Value
series_tooltip_toJson = \case
  (SeriesT_Line s) -> s ^? series_tooltip . _Just . to Aeson.toJSON
  _ -> Nothing

series_smooth_toJson :: SeriesT s -> Maybe Aeson.Value
series_smooth_toJson = \case
  (SeriesT_Line s) -> s ^? series_smooth . _Just . to (either Aeson.toJSON Aeson.toJSON)
  _ -> Nothing

series_areaStyle_toJson :: SeriesT s -> Maybe Aeson.Value
series_areaStyle_toJson = \case
  (SeriesT_Line s) -> s ^? series_areaStyle . _Just . to Aeson.toJSON
  _ -> Nothing

series_stack_toJson :: SeriesT s -> Maybe Aeson.Value
series_stack_toJson = \case
  (SeriesT_Line s) -> s ^? series_stack . _Just . to Aeson.toJSON
  _ -> Nothing

series_label_toJson :: SeriesT s -> Maybe Aeson.Value
series_label_toJson = \case
  (SeriesT_Line s) -> s ^? series_label . _Just . to Aeson.toJSON
  _ -> Nothing

series_animation_toJson :: SeriesT s -> Maybe Aeson.Value
series_animation_toJson = \case
  (SeriesT_Line s) -> s ^? series_animation . _Just . to Aeson.toJSON
  _ -> Nothing

series_lineStyle_toJson :: SeriesT s -> Maybe Aeson.Value
series_lineStyle_toJson = \case
  (SeriesT_Line s) -> s ^? series_lineStyle . _Just . to Aeson.toJSON
  _ -> Nothing

series_markArea_toJson :: SeriesT s -> Maybe Aeson.Value
series_markArea_toJson = \case
  (SeriesT_Line s) -> s ^? series_markArea . _Just . to Aeson.toJSON
  _ -> Nothing

series_markLine_toJson :: SeriesT s -> Maybe Aeson.Value
series_markLine_toJson = \case
  (SeriesT_Line s) -> s ^? series_markLine . _Just . to Aeson.toJSON
  _ -> Nothing

series_xAxisIndex_toJson :: SeriesT s -> Maybe Aeson.Value
series_xAxisIndex_toJson = \case
  (SeriesT_Line s) -> s ^? series_xAxisIndex . _Just . to Aeson.toJSON
  _ -> Nothing

series_yAxisIndex_toJson :: SeriesT s -> Maybe Aeson.Value
series_yAxisIndex_toJson = \case
  (SeriesT_Line s) -> s ^? series_yAxisIndex . _Just . to Aeson.toJSON
  _ -> Nothing

series_itemStyle_toJson :: SeriesT s -> Maybe Aeson.Value
series_itemStyle_toJson = \case
  (SeriesT_Line s) -> s ^? series_itemStyle . _Just . to Aeson.toJSON
  _ -> Nothing

series_symbol_toJson :: SeriesT s -> Maybe Aeson.Value
series_symbol_toJson = \case
  (SeriesT_Line s) -> s ^? series_symbol . _Just . to Aeson.toJSON
  _ -> Nothing

series_showSymbol_toJson :: SeriesT s -> Maybe Aeson.Value
series_showSymbol_toJson = \case
  (SeriesT_Line s) -> s ^? series_showSymbol . _Just . to Aeson.toJSON
  _ -> Nothing

series_symbolSize_toJson :: SeriesT s -> Maybe Aeson.Value
series_symbolSize_toJson = \case
  (SeriesT_Line s) -> s ^? series_symbolSize . _Just . to Aeson.toJSON
  _ -> Nothing

series_hoverAnimation_toJson :: SeriesT s -> Maybe Aeson.Value
series_hoverAnimation_toJson = \case
  (SeriesT_Line s) -> s ^? series_hoverAnimation . _Just . to Aeson.toJSON
  _ -> Nothing
