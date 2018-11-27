{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
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

import ECharts.Types
import ECharts.Internal.EChartTypes
import ECharts.Series
import ECharts.ChartOptions

data EChartSeries = EChartSeries
  -- line options
  { _eChartSeries_type :: Maybe Text
  , _eChartSeries_id :: Maybe Text
  , _eChartSeries_name :: Maybe Text
  , _eChartSeries_coordinateSystem :: Maybe CoordinateSystem
  , _eChartSeries_xAxisIndex :: Maybe Int
  , _eChartSeries_yAxisIndex :: Maybe Aeson.Value
  , _eChartSeries_polarIndex :: Maybe Int
  , _eChartSeries_symbol :: Maybe Symbol
  , _eChartSeries_symbolSize :: Maybe SymbolSize
  , _eChartSeries_symbolRotate :: Maybe Int
  , _eChartSeries_symbolKeepAspect :: Maybe Bool
  , _eChartSeries_symbolOffset :: Maybe (Int, Int) -- or Text?
  , _eChartSeries_showSymbol :: Maybe Bool
  , _eChartSeries_showAllSymbol :: Maybe Bool
  , _eChartSeries_hoverAnimation :: Maybe Bool
  , _eChartSeries_legendHoverLink :: Maybe Bool
  , _eChartSeries_stack :: Maybe Aeson.Value
  , _eChartSeries_cursor :: Maybe Text
  , _eChartSeries_connectNulls :: Maybe Bool
  , _eChartSeries_clipOverflow :: Maybe Bool
  , _eChartSeries_step :: Maybe Step
  , _eChartSeries_label :: Maybe Aeson.Value
  , _eChartSeries_itemStyle :: Maybe ItemStyle -- common style
  , _eChartSeries_lineStyle :: Maybe Aeson.Value
  , _eChartSeries_areaStyle :: Maybe Aeson.Value
  , _eChartSeries_emphasis :: Maybe Emphasis -- common
  , _eChartSeries_smooth :: Maybe Aeson.Value
  , _eChartSeries_smoothMonotone :: Maybe SmoothMonotone
  , _eChartSeries_sampling :: Maybe Sampling
  , _eChartSeries_dimensions :: Maybe Aeson.Value
  , _eChartSeries_encode :: Maybe Encode -- common
  , _eChartSeries_seriesLayoutBy :: Maybe Text
  , _eChartSeries_datasetindex :: Maybe Text
  , _eChartSeries_data :: Maybe Aeson.Value
  , _eChartSeries_markPoint :: Maybe MarkPoint -- common
  , _eChartSeries_markLine :: Maybe MarkLine -- common
  , _eChartSeries_markArea :: Maybe Aeson.Value
  , _eChartSeries_zlevel :: Maybe Int
  , _eChartSeries_z :: Maybe Int
  , _eChartSeries_animation :: Maybe Aeson.Value
  , _eChartSeries_animationOptions :: Maybe Animation
  , _eChartSeries_tooltip :: Maybe Aeson.Value -- common
  -- bar options
  -- candlestick has AbsOrPercentage
  -- , _eChartSeries_barWidth :: Maybe Int
  -- , _eChartSeries_barMaxWidth :: Maybe Int
  , _eChartSeries_barMinHeight :: Maybe Int
  , _eChartSeries_barGap :: Maybe Text
  , _eChartSeries_barCategoryGap :: Maybe Text
  , _eChartSeries_large :: Maybe Bool
  , _eChartSeries_largeThreshold :: Maybe Int
  , _eChartSeries_progressive :: Maybe Int
  , _eChartSeries_progressiveThreshold :: Maybe Int
  , _eChartSeries_progressiveChunkMode :: Maybe Text
  -- pie options
  , _eChartSeries_hoverOffset :: Maybe Int
  , _eChartSeries_selectedMode :: Maybe SelectedMode
  , _eChartSeries_selectedOffset :: Maybe Int
  , _eChartSeries_clockwise :: Maybe Bool
  , _eChartSeries_minAngle :: Maybe Int
  , _eChartSeries_roseType :: Maybe Text
  , _eChartSeries_avoidLabelOverlap :: Maybe Bool
  , _eChartSeries_stillShowZeroSum :: Maybe Bool
  , _eChartSeries_center :: Maybe (AbsOrPercentage, AbsOrPercentage)
  , _eChartSeries_radius :: Maybe (AbsOrPercentage, AbsOrPercentage)
  -- scatter options
  , _eChartSeries_geoIndex :: Maybe Int
  , _eChartSeries_calendarIndex :: Maybe Int
  -- effectScatter options
  , _eChartSeries_showEffectOn :: Maybe Text
  , _eChartSeries_rippleEffect :: Maybe RippleEffect
  -- radar options
  , _eChartSeries_radarIndex :: Maybe Int
  -- tree options
  , _eChartSeries_left :: Maybe TextOrScientific
  , _eChartSeries_top :: Maybe TextOrScientific
  , _eChartSeries_right :: Maybe TextOrScientific
  , _eChartSeries_bottom :: Maybe TextOrScientific
  , _eChartSeries_width :: Maybe TextOrScientific
  , _eChartSeries_height :: Maybe TextOrScientific
  , _eChartSeries_layout :: Maybe Text
  , _eChartSeries_orient :: Maybe Text
  , _eChartSeries_roam :: Maybe Text
  , _eChartSeries_expandAndCollapse :: Maybe Bool
  , _eChartSeries_initialTreeDepth :: Maybe Int
  , _eChartSeries_leaves :: (Maybe Label, Maybe ItemStyle, Maybe Emphasis)
  -- treemap options
  , _eChartSeries_squareRatio :: Maybe Scientific
  , _eChartSeries_leafDepth :: Maybe Int
  , _eChartSeries_drillDownIcon :: Maybe Text
  , _eChartSeries_nodeClick :: Maybe Text
  , _eChartSeries_zoomToNodeRatio :: Maybe Scientific
  , _eChartSeries_levels :: Maybe Aeson.Value
  , _eChartSeries_silent :: Maybe Aeson.Value
  , _eChartSeries_visualDimension :: Maybe Int
  , _eChartSeries_visualMin :: Maybe Int
  , _eChartSeries_visualMax :: Maybe Int
  , _eChartSeries_colorAlpha :: Maybe Aeson.Value
  , _eChartSeries_colorSaturation :: Maybe Aeson.Value
  , _eChartSeries_colorMappingsBy :: Maybe Text
  , _eChartSeries_visibleMin :: Maybe Int
  , _eChartSeries_childrenVisibleMin :: Maybe Int
  , _eChartSeries_breadcrumb :: Maybe Aeson.Value
  -- sunburst options
  , _eChartSeries_sort :: Maybe Aeson.Value
  , _eChartSeries_renderLabelForZeroData :: Maybe Bool
  , _eChartSeries_downplay :: Maybe Aeson.Value
  -- boxplot options
  , _eChartSeries_boxWidth :: Maybe (AbsOrPercentage, AbsOrPercentage)
  -- candlestick options
  , _eChartSeries_barWidth :: Maybe AbsOrPercentage
  , _eChartSeries_barMinWidth :: Maybe AbsOrPercentage
  , _eChartSeries_barMaxWidth :: Maybe AbsOrPercentage
  -- heatmap
  , _eChartSeries_blurSize :: Maybe Int
  , _eChartSeries_minOpacity :: Maybe Scientific
  , _eChartSeries_maxOpacity :: Maybe Scientific
  -- map
  , _eChartSeries_aspectScale :: Maybe Scientific
  , _eChartSeries_boundingCoords :: Maybe Aeson.Value
  , _eChartSeries_zoom :: Maybe Scientific
  , _eChartSeries_scaleLimit :: (Maybe Scientific, Maybe Scientific)
  , _eChartSeries_nameMap :: Maybe Aeson.Value
  , _eChartSeries_layoutCenter :: Maybe Aeson.Value
  , _eChartSeries_mapValueCalculation :: Maybe Text
  , _eChartSeries_showLegendSymbol :: Maybe Bool
  -- parallel
  , _eChartSeries_parallelIndex :: Maybe Int
  , _eChartSeries_inactiveOpacity :: Maybe Scientific
  , _eChartSeries_activeOpacity :: Maybe Scientific
  , _eChartSeries_realtime :: Maybe Bool
  -- lines
  , _eChartSeries_polyline :: Maybe Bool
  -- graph
  , _eChartSeries_circularRotateLayout :: Maybe Bool
  , _eChartSeries_force :: Maybe Aeson.Value
  , _eChartSeries_nodeScaleRatio :: Maybe Scientific
  , _eChartSeries_draggable :: Maybe Bool
  -- focusNodeAdjacency :: Maybe Bool -- sankey has more options
  , _eChartSeries_edgeSymbol :: Maybe Aeson.Value
  , _eChartSeries_edgeSymbolSize :: Maybe Aeson.Value
  , _eChartSeries_categories :: Maybe Aeson.Value
  , _eChartSeries_links :: Maybe Aeson.Value
  -- sankey
  , _eChartSeries_nodeWidth :: Maybe Scientific
  , _eChartSeries_nodeGap :: Maybe Scientific
  , _eChartSeries_layoutIterations :: Maybe Int
  , _eChartSeries_focusNodeAdjacency :: Maybe (Either Bool Text)
  -- funnel
  , _eChartSeries_min :: Maybe Scientific
  , _eChartSeries_max :: Maybe Scientific
  , _eChartSeries_minSize :: Maybe AbsOrPercentage
  , _eChartSeries_maxSize :: Maybe AbsOrPercentage
  , _eChartSeries_gap :: Maybe Scientific
  , _eChartSeries_funnelAlign :: Maybe Text
  , _eChartSeries_labelLine :: Maybe Aeson.Value
  -- gauge
  , _eChartSeries_startAngle :: Maybe Scientific
  , _eChartSeries_endAngle :: Maybe Scientific
  , _eChartSeries_splitNumber :: Maybe Scientific
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
  -- , _eChartSeries_coordinateSystem       = _series_coordinateSystem       s
  -- , _eChartSeries_xAxisIndex             = _series_xAxisIndex             s
  , _eChartSeries_yAxisIndex             = series_yAxisIndex_toJson st
  -- , _eChartSeries_polarIndex             = _series_polarIndex             s
  -- , _eChartSeries_symbol                 = _series_symbol                 s
  -- , _eChartSeries_symbolSize             = _series_symbolSize             s
  -- , _eChartSeries_symbolRotate           = _series_symbolRotate           s
  -- , _eChartSeries_symbolKeepAspect       = _series_symbolKeepAspect       s
  -- , _eChartSeries_symbolOffset           = _series_symbolOffset           s
  -- , _eChartSeries_showSymbol             = _series_showSymbol             s
  -- , _eChartSeries_showAllSymbol          = _series_showAllSymbol          s
  -- , _eChartSeries_hoverAnimation         = _series_hoverAnimation         s
  -- , _eChartSeries_legendHoverLink        = _series_legendHoverLink        s
  , _eChartSeries_stack                  = series_stack_toJson st
  -- , _eChartSeries_cursor                 = _series_cursor                 s
  -- , _eChartSeries_connectNulls           = _series_connectNulls           s
  -- , _eChartSeries_clipOverflow           = _series_clipOverflow           s
  -- , _eChartSeries_step                   = _series_step                   s
  , _eChartSeries_label                  = series_label_toJson st
  -- , _eChartSeries_itemStyle              = _series_itemStyle              s
  , _eChartSeries_lineStyle              = series_lineStyle_toJson st
  , _eChartSeries_areaStyle              = series_areaStyle_toJson st
  -- , _eChartSeries_emphasis               = _series_emphasis               s
  , _eChartSeries_smooth                 = series_smooth_toJson st
  -- , _eChartSeries_smoothMonotone         = _series_smoothMonotone         s
  -- , _eChartSeries_sampling               = _series_sampling               s
  -- , _eChartSeries_dimensions             = _series_dimensions             s
  -- , _eChartSeries_encode                 = _series_encode                 s
  -- , _eChartSeries_seriesLayoutBy         = _series_seriesLayoutBy         s
  -- , _eChartSeries_datasetindex           = _series_datasetindex           s
  -- , _eChartSeries_markPoint              = _series_markPoint              s
  -- , _eChartSeries_markLine               = _series_markLine               s
  , _eChartSeries_markArea               = series_markArea_toJson st
  -- , _eChartSeries_zlevel                 = _series_zlevel                 s
  -- , _eChartSeries_z                      = _series_z                      s
  , _eChartSeries_animation              = series_animation_toJson st
  -- , _eChartSeries_animationOptions       = _series_animationOptions       s
  -- , _eChartSeries_tooltip                = _series_tooltip                s
  -- , _eChartSeries_barMinHeight           = _series_barMinHeight           s
  -- , _eChartSeries_barGap                 = _series_barGap                 s
  -- , _eChartSeries_barCategoryGap         = _series_barCategoryGap         s
  -- , _eChartSeries_large                  = _series_large                  s
  -- , _eChartSeries_largeThreshold         = _series_largeThreshold         s
  -- , _eChartSeries_progressive            = _series_progressive            s
  -- , _eChartSeries_progressiveThreshold   = _series_progressiveThreshold   s
  -- , _eChartSeries_progressiveChunkMode   = _series_progressiveChunkMode   s
  -- , _eChartSeries_hoverOffset            = _series_hoverOffset            s
  -- , _eChartSeries_selectedMode           = _series_selectedMode           s
  -- , _eChartSeries_selectedOffset         = _series_selectedOffset         s
  -- , _eChartSeries_clockwise              = _series_clockwise              s
  -- , _eChartSeries_minAngle               = _series_minAngle               s
  -- , _eChartSeries_roseType               = _series_roseType               s
  -- , _eChartSeries_avoidLabelOverlap      = _series_avoidLabelOverlap      s
  -- , _eChartSeries_stillShowZeroSum       = _series_stillShowZeroSum       s
  -- , _eChartSeries_center                 = _series_center                 s
  -- , _eChartSeries_radius                 = _series_radius                 s
  -- , _eChartSeries_geoIndex               = _series_geoIndex               s
  -- , _eChartSeries_calendarIndex          = _series_calendarIndex          s
  -- , _eChartSeries_showEffectOn           = _series_showEffectOn           s
  -- , _eChartSeries_rippleEffect           = _series_rippleEffect           s
  -- , _eChartSeries_radarIndex             = _series_radarIndex             s
  -- , _eChartSeries_left                   = _series_left                   s
  -- , _eChartSeries_top                    = _series_top                    s
  -- , _eChartSeries_right                  = _series_right                  s
  -- , _eChartSeries_bottom                 = _series_bottom                 s
  -- , _eChartSeries_width                  = _series_width                  s
  -- , _eChartSeries_height                 = _series_height                 s
  -- , _eChartSeries_layout                 = _series_layout                 s
  -- , _eChartSeries_orient                 = _series_orient                 s
  -- , _eChartSeries_roam                   = _series_roam                   s
  -- , _eChartSeries_expandAndCollapse      = _series_expandAndCollapse      s
  -- , _eChartSeries_initialTreeDepth       = _series_initialTreeDepth       s
  -- , _eChartSeries_leaves                 = _series_leaves                 s
  -- , _eChartSeries_squareRatio            = _series_squareRatio            s
  -- , _eChartSeries_leafDepth              = _series_leafDepth              s
  -- , _eChartSeries_drillDownIcon          = _series_drillDownIcon          s
  -- , _eChartSeries_nodeClick              = _series_nodeClick              s
  -- , _eChartSeries_zoomToNodeRatio        = _series_zoomToNodeRatio        s
  -- , _eChartSeries_levels                 = _series_levels                 s
  -- , _eChartSeries_silent                 = _series_silent                 s
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
