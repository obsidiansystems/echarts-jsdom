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
module ECharts.Series
  ( module ECharts.Series
  , getSeriesType
  )
  where

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
import ECharts.Series.Internal

data SeriesT s where
  SeriesT_Line :: Series SeriesLine -> SeriesT SeriesLine
  SeriesT_Pie :: Series SeriesPie -> SeriesT SeriesPie

getSeries :: SeriesT s -> Series s
getSeries (SeriesT_Line s) = s
getSeries (SeriesT_Pie s) = s

getSeriesType :: forall s . SeriesT s -> Text
getSeriesType (SeriesT_Line _) = getSeriesTypeInt (Proxy :: Proxy s)
getSeriesType (SeriesT_Pie _) = getSeriesTypeInt (Proxy :: Proxy s)

data Series seriesType = Series
  -- Common options
  { _series_name                   :: Maybe Text
  , _series_id                     :: Maybe Text
  , _series_color                  :: Maybe [Text]
  , _series_backgroundColor        :: Maybe [Text]
  -- SeriesOptions based on seriesType
  , _series_coordinateSystem       :: SeriesOptions_coordinateSystem seriesType
  , _series_xAxisIndex             :: SeriesOptions_xAxisIndex seriesType
  , _series_yAxisIndex             :: SeriesOptions_yAxisIndex seriesType
  , _series_polarIndex             :: SeriesOptions_polarIndex seriesType
  , _series_symbol                 :: SeriesOptions_symbol seriesType
  , _series_symbolSize             :: SeriesOptions_symbolSize seriesType
  , _series_symbolRotate           :: SeriesOptions_symbolRotate seriesType
  , _series_symbolKeepAspect       :: SeriesOptions_symbolKeepAspect seriesType
  , _series_symbolOffset           :: SeriesOptions_symbolOffset seriesType
  , _series_showSymbol             :: SeriesOptions_showSymbol seriesType
  , _series_showAllSymbol          :: SeriesOptions_showAllSymbol seriesType
  , _series_hoverAnimation         :: SeriesOptions_hoverAnimation seriesType
  , _series_legendHoverLink        :: SeriesOptions_legendHoverLink seriesType
  , _series_stack                  :: SeriesOptions_stack seriesType
  , _series_cursor                 :: SeriesOptions_cursor seriesType
  , _series_connectNulls           :: SeriesOptions_connectNulls seriesType
  , _series_clipOverflow           :: SeriesOptions_clipOverflow seriesType
  , _series_step                   :: SeriesOptions_step seriesType
  , _series_label                  :: SeriesOptions_label seriesType
  , _series_itemStyle              :: SeriesOptions_itemStyle seriesType
  , _series_lineStyle              :: SeriesOptions_lineStyle seriesType
  , _series_areaStyle              :: SeriesOptions_areaStyle seriesType
  , _series_emphasis               :: SeriesOptions_emphasis seriesType
  , _series_smooth                 :: SeriesOptions_smooth seriesType
  , _series_smoothMonotone         :: SeriesOptions_smoothMonotone seriesType
  , _series_sampling               :: SeriesOptions_sampling seriesType
  , _series_dimensions             :: SeriesOptions_dimensions seriesType
  , _series_encode                 :: SeriesOptions_encode seriesType
  , _series_seriesLayoutBy         :: SeriesOptions_seriesLayoutBy seriesType
  , _series_datasetindex           :: SeriesOptions_datasetindex seriesType
  , _series_data                   :: SeriesOptions_data seriesType
  , _series_markPoint              :: SeriesOptions_markPoint seriesType
  , _series_markLine               :: SeriesOptions_markLine seriesType
  , _series_markArea               :: SeriesOptions_markArea seriesType
  , _series_zlevel                 :: SeriesOptions_zlevel seriesType
  , _series_z                      :: SeriesOptions_z seriesType
  , _series_animation              :: SeriesOptions_animation seriesType
  , _series_animationOptions       :: SeriesOptions_animationOptions seriesType
  , _series_tooltip                :: SeriesOptions_tooltip seriesType
  , _series_barMinHeight           :: SeriesOptions_barMinHeight seriesType
  , _series_barGap                 :: SeriesOptions_barGap seriesType
  , _series_barCategoryGap         :: SeriesOptions_barCategoryGap seriesType
  , _series_large                  :: SeriesOptions_large seriesType
  , _series_largeThreshold         :: SeriesOptions_largeThreshold seriesType
  , _series_progressive            :: SeriesOptions_progressive seriesType
  , _series_progressiveThreshold   :: SeriesOptions_progressiveThreshold seriesType
  , _series_progressiveChunkMode   :: SeriesOptions_progressiveChunkMode seriesType
  , _series_hoverOffset            :: SeriesOptions_hoverOffset seriesType
  , _series_selectedMode           :: SeriesOptions_selectedMode seriesType
  , _series_selectedOffset         :: SeriesOptions_selectedOffset seriesType
  , _series_clockwise              :: SeriesOptions_clockwise seriesType
  , _series_minAngle               :: SeriesOptions_minAngle seriesType
  , _series_roseType               :: SeriesOptions_roseType seriesType
  , _series_avoidLabelOverlap      :: SeriesOptions_avoidLabelOverlap seriesType
  , _series_stillShowZeroSum       :: SeriesOptions_stillShowZeroSum seriesType
  , _series_center                 :: SeriesOptions_center seriesType
  , _series_radius                 :: SeriesOptions_radius seriesType
  , _series_geoIndex               :: SeriesOptions_geoIndex seriesType
  , _series_calendarIndex          :: SeriesOptions_calendarIndex seriesType
  , _series_showEffectOn           :: SeriesOptions_showEffectOn seriesType
  , _series_rippleEffect           :: SeriesOptions_rippleEffect seriesType
  , _series_radarIndex             :: SeriesOptions_radarIndex seriesType
  , _series_left                   :: SeriesOptions_left seriesType
  , _series_top                    :: SeriesOptions_top seriesType
  , _series_right                  :: SeriesOptions_right seriesType
  , _series_bottom                 :: SeriesOptions_bottom seriesType
  , _series_width                  :: SeriesOptions_width seriesType
  , _series_height                 :: SeriesOptions_height seriesType
  , _series_layout                 :: SeriesOptions_layout seriesType
  , _series_orient                 :: SeriesOptions_orient seriesType
  , _series_roam                   :: SeriesOptions_roam seriesType
  , _series_expandAndCollapse      :: SeriesOptions_expandAndCollapse seriesType
  , _series_initialTreeDepth       :: SeriesOptions_initialTreeDepth seriesType
  , _series_leaves                 :: SeriesOptions_leaves seriesType
  , _series_squareRatio            :: SeriesOptions_squareRatio seriesType
  , _series_leafDepth              :: SeriesOptions_leafDepth seriesType
  , _series_drillDownIcon          :: SeriesOptions_drillDownIcon seriesType
  , _series_nodeClick              :: SeriesOptions_nodeClick seriesType
  , _series_zoomToNodeRatio        :: SeriesOptions_zoomToNodeRatio seriesType
  , _series_levels                 :: SeriesOptions_levels seriesType
  , _series_silent                 :: SeriesOptions_silent seriesType
  , _series_visualDimension        :: SeriesOptions_visualDimension seriesType
  , _series_visualMin              :: SeriesOptions_visualMin seriesType
  , _series_visualMax              :: SeriesOptions_visualMax seriesType
  , _series_colorAlpha             :: SeriesOptions_colorAlpha seriesType
  , _series_colorSaturation        :: SeriesOptions_colorSaturation seriesType
  , _series_colorMappingsBy        :: SeriesOptions_colorMappingsBy seriesType
  , _series_visibleMin             :: SeriesOptions_visibleMin seriesType
  , _series_childrenVisibleMin     :: SeriesOptions_childrenVisibleMin seriesType
  , _series_breadcrumb             :: SeriesOptions_breadcrumb seriesType
  , _series_sort                   :: SeriesOptions_sort seriesType
  , _series_renderLabelForZeroData :: SeriesOptions_renderLabelForZeroData seriesType
  , _series_downplay               :: SeriesOptions_downplay seriesType
  , _series_boxWidth               :: SeriesOptions_boxWidth seriesType
  , _series_barWidth               :: SeriesOptions_barWidth seriesType
  , _series_barMinWidth            :: SeriesOptions_barMinWidth seriesType
  , _series_barMaxWidth            :: SeriesOptions_barMaxWidth seriesType
  , _series_blurSize               :: SeriesOptions_blurSize seriesType
  , _series_minOpacity             :: SeriesOptions_minOpacity seriesType
  , _series_maxOpacity             :: SeriesOptions_maxOpacity seriesType
  , _series_aspectScale            :: SeriesOptions_aspectScale seriesType
  , _series_boundingCoords         :: SeriesOptions_boundingCoords seriesType
  , _series_zoom                   :: SeriesOptions_zoom seriesType
  , _series_scaleLimit             :: SeriesOptions_scaleLimit seriesType
  , _series_nameMap                :: SeriesOptions_nameMap seriesType
  , _series_layoutCenter           :: SeriesOptions_layoutCenter seriesType
  , _series_mapValueCalculation    :: SeriesOptions_mapValueCalculation seriesType
  , _series_showLegendSymbol       :: SeriesOptions_showLegendSymbol seriesType
  , _series_parallelIndex          :: SeriesOptions_parallelIndex seriesType
  , _series_inactiveOpacity        :: SeriesOptions_inactiveOpacity seriesType
  , _series_activeOpacity          :: SeriesOptions_activeOpacity seriesType
  , _series_realtime               :: SeriesOptions_realtime seriesType
  , _series_polyline               :: SeriesOptions_polyline seriesType
  , _series_circularRotateLayout   :: SeriesOptions_circularRotateLayout seriesType
  , _series_force                  :: SeriesOptions_force seriesType
  , _series_nodeScaleRatio         :: SeriesOptions_nodeScaleRatio seriesType
  , _series_draggable              :: SeriesOptions_draggable seriesType
  , _series_edgeSymbol             :: SeriesOptions_edgeSymbol seriesType
  , _series_edgeSymbolSize         :: SeriesOptions_edgeSymbolSize seriesType
  , _series_categories             :: SeriesOptions_categories seriesType
  , _series_links                  :: SeriesOptions_links seriesType
  , _series_nodeWidth              :: SeriesOptions_nodeWidth seriesType
  , _series_nodeGap                :: SeriesOptions_nodeGap seriesType
  , _series_layoutIterations       :: SeriesOptions_layoutIterations seriesType
  , _series_focusNodeAdjacency     :: SeriesOptions_focusNodeAdjacency seriesType
  , _series_min                    :: SeriesOptions_min seriesType
  , _series_max                    :: SeriesOptions_max seriesType
  , _series_minSize                :: SeriesOptions_minSize seriesType
  , _series_maxSize                :: SeriesOptions_maxSize seriesType
  , _series_gap                    :: SeriesOptions_gap seriesType
  , _series_funnelAlign            :: SeriesOptions_funnelAlign seriesType
  , _series_labelLine              :: SeriesOptions_labelLine seriesType
  , _series_startAngle             :: SeriesOptions_startAngle seriesType
  , _series_endAngle               :: SeriesOptions_endAngle seriesType
  , _series_splitNumber            :: SeriesOptions_splitNumber seriesType
  , _series_axisLine               :: SeriesOptions_axisLine seriesType
  , _series_splitLine              :: SeriesOptions_splitLine seriesType
  , _series_axisTick               :: SeriesOptions_axisTick seriesType
  , _series_axisLabel              :: SeriesOptions_axisLabel seriesType
  , _series_pointer                :: SeriesOptions_pointer seriesType
  , _series_details                :: SeriesOptions_details seriesType
  }
  deriving (Generic)

instance Default (Series SeriesLine) where

makeLenses ''Series

series_data_toJson :: SeriesT s -> Maybe Aeson.Value
series_data_toJson = \case
  (SeriesT_Line s) -> s ^? series_data . _Just . to Aeson.toJSON
  (SeriesT_Pie s)  -> s ^? series_data . _Just . to Aeson.toJSON
  _ -> Nothing

