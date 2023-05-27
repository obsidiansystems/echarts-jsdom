{-# LANGUAGE TypeFamilies #-}
module ECharts.Data.Internal where

import Data.Text (Text)

import ECharts.Types

class DataOptions seriesType where
  type DataOptions_name seriesType
  type DataOptions_value seriesType
  type DataOptions_symbol seriesType
  type DataOptions_symbolSize seriesType
  type DataOptions_symbolRotate seriesType
  type DataOptions_symbolKeepAspect seriesType
  type DataOptions_symbolOffset seriesType
  type DataOptions_label seriesType
  type DataOptions_itemStyle seriesType
  type DataOptions_emphasis seriesType
  type DataOptions_tooltip seriesType
  type DataOptions_selected seriesType
  type DataOptions_labelLine seriesType
  type DataOptions_areaStyle seriesType
  type DataOptions_animation seriesType
  type DataOptions_id seriesType
  type DataOptions_visualDimension seriesType
  type DataOptions_visualMin seriesType
  type DataOptions_visualMax seriesType
  type DataOptions_color seriesType
  type DataOptions_colorAlpha seriesType
  type DataOptions_colorSaturation seriesType
  type DataOptions_colorMappingsBy seriesType
  type DataOptions_childrenVisibleMin seriesType
  type DataOptions_upperLabel seriesType
  type DataOptions_link seriesType
  type DataOptions_target seriesType
  type DataOptions_highlight seriesType
  type DataOptions_downplay seriesType
  type DataOptions_children seriesType
  type DataOptions_width seriesType
  type DataOptions_type seriesType
  type DataOptions_shadowBlur seriesType
  type DataOptions_shadowColor seriesType
  type DataOptions_shadowOffsetX seriesType
  type DataOptions_shadowOffsetY seriesType
  type DataOptions_opacity seriesType
  type DataOptions_coords seriesType
  type DataOptions_x seriesType
  type DataOptions_y seriesType
  type DataOptions_z seriesType
  type DataOptions_fixed seriesType
  type DataOptions_category seriesType
  type DataOptions_hoverAnimation seriesType

  type instance DataOptions_name seriesType = ()
  type instance DataOptions_value seriesType = ()
  type instance DataOptions_symbol seriesType = ()
  type instance DataOptions_symbolSize seriesType = ()
  type instance DataOptions_symbolRotate seriesType = ()
  type instance DataOptions_symbolKeepAspect seriesType = ()
  type instance DataOptions_symbolOffset seriesType = ()
  type instance DataOptions_label seriesType = ()
  type instance DataOptions_itemStyle seriesType = ()
  type instance DataOptions_emphasis seriesType = ()
  type instance DataOptions_tooltip seriesType = ()
  type instance DataOptions_selected seriesType = ()
  type instance DataOptions_labelLine seriesType = ()
  type instance DataOptions_areaStyle seriesType = ()
  type instance DataOptions_animation seriesType = ()
  type instance DataOptions_id seriesType = ()
  type instance DataOptions_visualDimension seriesType = ()
  type instance DataOptions_visualMin seriesType = ()
  type instance DataOptions_visualMax seriesType = ()
  type instance DataOptions_color seriesType = ()
  type instance DataOptions_colorAlpha seriesType = ()
  type instance DataOptions_colorSaturation seriesType = ()
  type instance DataOptions_colorMappingsBy seriesType = ()
  type instance DataOptions_childrenVisibleMin seriesType = ()
  type instance DataOptions_upperLabel seriesType = ()
  type instance DataOptions_link seriesType = ()
  type instance DataOptions_target seriesType = ()
  type instance DataOptions_highlight seriesType = ()
  type instance DataOptions_downplay seriesType = ()
  type instance DataOptions_children seriesType = ()
  type instance DataOptions_width seriesType = ()
  type instance DataOptions_type seriesType = ()
  type instance DataOptions_shadowBlur seriesType = ()
  type instance DataOptions_shadowColor seriesType = ()
  type instance DataOptions_shadowOffsetX seriesType = ()
  type instance DataOptions_shadowOffsetY seriesType = ()
  type instance DataOptions_opacity seriesType = ()
  type instance DataOptions_coords seriesType = ()
  type instance DataOptions_x seriesType = ()
  type instance DataOptions_y seriesType = ()
  type instance DataOptions_z seriesType = ()
  type instance DataOptions_fixed seriesType = ()
  type instance DataOptions_category seriesType = ()
  type instance DataOptions_hoverAnimation seriesType = ()

instance DataOptions SeriesLine where
  type DataOptions_name SeriesLine = Maybe Int
  type DataOptions_value SeriesLine = Maybe (Int, Double)

instance DataOptions SeriesPie where
  type DataOptions_name SeriesPie = Maybe Text

instance DataOptions SeriesBar where
  type DataOptions_name SeriesBar = Maybe Text
