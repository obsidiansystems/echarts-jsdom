{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
module ECharts.Internal where

import Control.Monad (join)
import Data.Default (Default, def)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as Aeson.KeyMap
import qualified Data.Aeson.Key as Aeson.Key
import qualified Data.Text as T
import qualified Data.Map as Map
import GHC.Generics (Generic)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Vector as V
import Control.Lens
import Language.Javascript.JSaddle

import ECharts.Types
import ECharts.ChartOptions

import ECharts.DeriveToJSVal (toJSVal_generic, ToJSVal(..))
import ECharts.Internal.EChartSeries
import ECharts.Internal.EChartTypes
import ECharts.Internal.EChartToolTip
import ECharts.Internal.EChartToolBox
import ECharts.Internal.EChartGrid
import ECharts.Internal.EChartLegend
import ECharts.Internal.EChartTitle

data EChartConfig = EChartConfig
  { _eChartConfig_title :: Maybe EChartTitle
  , _eChartConfig_legend :: Maybe EChartLegend
  , _eChartConfig_tooltip :: Maybe EChartToolTip
  , _eChartConfig_axisPointer :: Maybe AxisPointer
  , _eChartConfig_toolbox :: Maybe EChartToolBox
  , _eChartConfig_dataZoom :: [EChartDataZoom]
  , _eChartConfig_visualMap :: Maybe [EChartVisualMap]
  , _eChartConfig_grid :: Maybe [EChartGrid]
  , _eChartConfig_xAxis :: [EChartAxis]
  , _eChartConfig_yAxis :: [EChartAxis]
  , _eChartConfig_series :: [EChartSeries]
  }
  deriving (Generic)

makeLenses ''EChartConfig

instance Default EChartConfig where

instance ToJSVal EChartConfig where
  toJSVal = toJSVal_generic (drop $ T.length "_eChartConfig_")

onlyNonEmpty :: [a] -> Maybe [a]
onlyNonEmpty = \case
  [] -> Nothing
  vs -> Just vs

toEChartConfig :: ChartOptions -> JSM EChartConfig
toEChartConfig c = def
  { _eChartConfig_title = toEChartTitle <$> _chartOptions_title c
  , _eChartConfig_legend = toEChartLegend <$> _chartOptions_legend c
  , _eChartConfig_tooltip = toEChartToolTip <$> _chartOptions_tooltip c
  , _eChartConfig_axisPointer = _chartOptions_axisPointer c
  , _eChartConfig_toolbox = toEChartToolBox <$> _chartOptions_toolbox c
  , _eChartConfig_dataZoom = toEChartDataZoom <$> _chartOptions_dataZoom c
  , _eChartConfig_visualMap = onlyNonEmpty (toEChartVisualMap <$> _chartOptions_visualMap c)
  , _eChartConfig_grid = onlyNonEmpty (toEChartGrid <$> _chartOptions_grid c)
  , _eChartConfig_xAxis = fmap toEChartAxis $ _chartOptions_xAxis c
  , _eChartConfig_yAxis = fmap toEChartAxis $ _chartOptions_yAxis c
  } & eChartConfig_series %%~ \_ -> mapM toEChartSeries $ _chartOptions_series c
  where
    toEChartTitle :: Title -> EChartTitle
    toEChartTitle t = EChartTitle
      { _eChartTitle_show = _title_show t
      , _eChartTitle_text = _title_text t
      , _eChartTitle_link = _title_link t
      , _eChartTitle_target = _title_target t
      , _eChartTitle_textStyle = fmap toEChartTextStyle $ _title_textStyle t
      , _eChartTitle_subtext = _title_subtext t
      , _eChartTitle_sublink = _title_sublink t
      , _eChartTitle_subtarget = _title_subtarget t
      , _eChartTitle_subtextStyle = fmap toEChartTextStyle $ _title_subtextStyle t
      , _eChartTitle_triggerEvent = _title_triggerEvent t
      , _eChartTitle_padding = _title_padding t
      , _eChartTitle_itemGap = _title_itemGap t
      , _eChartTitle_zlevel = join $ fmap _pos_zlevel $ _title_pos t
      , _eChartTitle_z = join $ fmap _pos_z $ _title_pos t
      , _eChartTitle_left = fmap posAlignToSN $ join $ fmap _pos_left $ _title_pos t
      , _eChartTitle_right = fmap posAlignToSN $ join $ fmap _pos_right $ _title_pos t
      , _eChartTitle_top = fmap posAlignToSN $ join $ fmap _pos_top $ _title_pos t
      , _eChartTitle_bottom = fmap posAlignToSN $ join $ fmap _pos_bottom $ _title_pos t
      , _eChartTitle_backgroundColor = _title_backgroundColor t
      , _eChartTitle_borderColor = join $ fmap _border_color $ _title_border t
      , _eChartTitle_borderWidth = join $ fmap _border_width $ _title_border t
      , _eChartTitle_borderRadius = join $ fmap _border_radius $ _title_border t
      , _eChartTitle_shadowBlur = join $ fmap _shadow_blur $ _title_shadow t
      , _eChartTitle_shadowColor = join $ fmap _shadow_color $ _title_shadow t
      , _eChartTitle_shadowOffsetX = join $ fmap _shadow_offsetX $ _title_shadow t
      , _eChartTitle_shadowOffsetY = join $ fmap _shadow_offsetY $ _title_shadow t
      }
    fromPos a b = fmap posAlignToSN $ join $ fmap a b
    toEChartLegend x = EChartLegend
      { _eChartLegend_type = _legend_type x
      , _eChartLegend_show = _legend_show x
      , _eChartLegend_zlevel = join $ fmap _pos_zlevel $ _legend_pos x
      , _eChartLegend_z = join $ fmap _pos_z $ _legend_pos x
      , _eChartLegend_left = fromPos _pos_left $ _legend_pos x
      , _eChartLegend_top = fromPos _pos_top $ _legend_pos x
      , _eChartLegend_right = fromPos _pos_right $ _legend_pos x
      , _eChartLegend_bottom = fromPos _pos_bottom $ _legend_pos x
      , _eChartLegend_width = fmap sizeValueToSN $ join $ fmap _size_width $ _legend_size x
      , _eChartLegend_height = fmap sizeValueToSN $ join $ fmap _size_height $ _legend_size x
      , _eChartLegend_orient = _legend_orient x
      , _eChartLegend_align = _legend_align x
      , _eChartLegend_padding = _legend_padding x
      , _eChartLegend_itemGap = _legend_itemGap x
      , _eChartLegend_itemWidth = _legend_itemWidth x
      , _eChartLegend_itemHeight = _legend_itemHeight x
      , _eChartLegend_symbolKeepAspect = _legend_symbolKeepAspect x
      , _eChartLegend_formatter = _legend_formatter x
      , _eChartLegend_selectedMode = _legend_selectedMode x
      , _eChartLegend_inactiveColor = _legend_inactiveColor x
      , _eChartLegend_selected =
        flip fmap (_legend_selected x) $ \s -> Aeson.Object $
          Aeson.KeyMap.fromList $ fmap (\(k, v) -> (Aeson.Key.fromText k, Aeson.toJSON v)) $ Map.toList s
      , _eChartLegend_textStyle = fmap toEChartTextStyle $ _legend_textStyle x
      , _eChartLegend_data =
        let toLegendDataObject (k, v) = Aeson.Object $ Aeson.KeyMap.mapMaybe id $ Aeson.KeyMap.fromList
              [ ("name", Just $ Aeson.toJSON k)
              , ("icon", fmap Aeson.toJSON $ _legendData_icon v)
              , ("textStyle", Aeson.toJSON . toEChartTextStyle <$> _legendData_textStyle v)
              ]
        in  flip fmap (_legend_data x) $ \d ->
              Aeson.Array $ fmap toLegendDataObject $ V.fromList d
      , _eChartLegend_backgroundColor = _legend_backgroundColor x
      , _eChartLegend_borderColor = join $ fmap _border_color $ _legend_border x
      , _eChartLegend_borderWidth = join $ fmap _border_width $ _legend_border x
      , _eChartLegend_borderRadius = join $ fmap _border_radius $ _legend_border x
      , _eChartLegend_shadowBlur = join $ fmap _shadow_blur $ _legend_shadow x
      , _eChartLegend_shadowColor = join $ fmap _shadow_color $ _legend_shadow x
      , _eChartLegend_shadowOffsetX = join $ fmap _shadow_offsetX $ _legend_shadow x
      , _eChartLegend_shadowOffsetY = join $ fmap _shadow_offsetY $ _legend_shadow x
      , _eChartLegend_scrollDataIndex = _legend_scrollDataIndex x
      , _eChartLegend_pageButtonItemGap = _legend_pageButtonItemGap x
      , _eChartLegend_pageButtonGap = _legend_pageButtonGap x
      , _eChartLegend_pageButtonPosition = _legend_pageButtonPosition x
      , _eChartLegend_pageFormatter = _legend_pageFormatter x
      , _eChartLegend_pageTextStyle = fmap toEChartTextStyle $ _legend_pageTextStyle x
      , _eChartLegend_animation = _legend_animation x
      , _eChartLegend_animationDurationUpdate = _legend_animationDurationUpdate x
      }

toEChartAxis :: Axis -> EChartAxis
toEChartAxis x = EChartAxis
  { _eChartAxis_show = _axis_show x
  , _eChartAxis_gridIndex = _axis_gridIndex x
  , _eChartAxis_zlevel = _axis_zlevel x
  , _eChartAxis_z = _axis_z x
  , _eChartAxis_offset = _axis_offset x
  , _eChartAxis_type = _axis_type x
  , _eChartAxis_name = _axis_name x
  , _eChartAxis_nameLocation = _axis_nameLocation x
  , _eChartAxis_nameTextStyle = fmap toEChartTextStyle $ _axis_nameTextStyle x
  , _eChartAxis_nameGap = _axis_nameGap x
  , _eChartAxis_nameRotate = _axis_nameRotate x
  , _eChartAxis_inverse = _axis_inverse x
  , _eChartAxis_boundaryGap = case _axis_boundaryGap x of
      Nothing -> Nothing
      Just (Left gap) -> Just $ Aeson.Bool gap
      Just (Right (a, b)) -> Just $ Aeson.toJSON (sizeValueToSN a, sizeValueToSN b)
  , _eChartAxis_min = ffor (_axis_min x) $ \case
      Right () -> SN_String "dataMin"
      Left n -> SN_Number n
  , _eChartAxis_max = ffor (_axis_max x) $ \case
      Right () -> SN_String "dataMax"
      Left n -> SN_Number n
  , _eChartAxis_scale = _axis_scale x
  , _eChartAxis_minInterval = _axis_minInterval x
  , _eChartAxis_interval = _axis_interval x
  , _eChartAxis_logBase = _axis_logBase x
  , _eChartAxis_silent = _axis_silent x
  , _eChartAxis_triggerEvent = _axis_triggerEvent x
  , _eChartAxis_splitLine = _axis_splitLine x
  , _eChartAxis_splitArea = _axis_splitArea x
  , _eChartAxis_splitNumber = _axis_splitNumber x
  , _eChartAxis_axisLine = fmap toEChartAxisLine $ _axis_axisLine x
  , _eChartAxis_axisTick = fmap toEChartAxisTick $ _axis_axisTick x
  , _eChartAxis_axisLabel = fmap toEChartAxisLabel $ _axis_axisLabel x
  , _eChartAxis_position = _axis_position x
  , _eChartAxis_data = case _axis_data x of
      Nothing -> Nothing
      Just d -> Just $ Aeson.Array $ V.fromList $
        fmap (\(k, v) -> Aeson.Object $ Aeson.KeyMap.fromList
          [ ("value", Aeson.toJSON k)
          , ("textStyle", Aeson.toJSON $ fmap toEChartTextStyle v)
          ]) d
  }

toEChartAxisLabel :: AxisLabel -> EChartAxisLabel
toEChartAxisLabel x = EChartAxisLabel
  { _eChartAxisLabel_show = _axisLabel_show x
  , _eChartAxisLabel_inside = _axisLabel_inside x
  , _eChartAxisLabel_rotate = _axisLabel_rotate x
  , _eChartAxisLabel_margin = _axisLabel_margin x
  , _eChartAxisLabel_showMinLabel = _axisLabel_showMinLabel x
  , _eChartAxisLabel_showMaxLabel = _axisLabel_showMaxLabel x
  , _eChartAxisLabel_fontStyle = join $ fmap _font_style $ _axisLabel_font x
  , _eChartAxisLabel_fontWeight = join $ fmap _font_weight $ _axisLabel_font x
  , _eChartAxisLabel_fontFamily = join $ fmap _font_family $ _axisLabel_font x
  , _eChartAxisLabel_fontSize = join $ fmap _font_size $ _axisLabel_font x
  , _eChartAxisLabel_align = _axisLabel_align x
  , _eChartAxisLabel_verticalAlign = _axisLabel_verticalAlign x
  , _eChartAxisLabel_lineHeight = _axisLabel_lineHeight x
  , _eChartAxisLabel_backgroundColor = _axisLabel_backgroundColor x
  , _eChartAxisLabel_borderColor = join $ fmap _border_color $ _axisLabel_border x
  , _eChartAxisLabel_borderWidth = join $ fmap _border_width $ _axisLabel_border x
  , _eChartAxisLabel_borderRadius = join $ fmap _border_radius $ _axisLabel_border x
  , _eChartAxisLabel_shadowBlur = join $ fmap _shadow_blur $ _axisLabel_shadow x
  , _eChartAxisLabel_shadowColor = join $ fmap _shadow_color $ _axisLabel_shadow x
  , _eChartAxisLabel_shadowOffsetX = join $ fmap _shadow_offsetX $ _axisLabel_shadow x
  , _eChartAxisLabel_shadowOffsetY = join $ fmap _shadow_offsetY $ _axisLabel_shadow x
  , _eChartAxisLabel_padding = _axisLabel_padding x
  , _eChartAxisLabel_width = fmap sizeValueToSN $ join $ fmap _size_width $ _axisLabel_size x
  , _eChartAxisLabel_height = fmap sizeValueToSN $ join $ fmap _size_height $ _axisLabel_size x
  , _eChartAxisLabel_textBorderColor = join $ fmap _border_color $ _axisLabel_textBorder x
  , _eChartAxisLabel_textBorderWidth = join $ fmap _border_width $ _axisLabel_textBorder x
  , _eChartAxisLabel_textBorderRadius = join $ fmap _border_radius $ _axisLabel_textBorder x
  , _eChartAxisLabel_textShadowColor = join $ fmap _shadow_color $ _axisLabel_textShadow x
  , _eChartAxisLabel_textShadowBlur = join $ fmap _shadow_blur $ _axisLabel_textShadow x
  , _eChartAxisLabel_textShadowOffsetX = join $ fmap _shadow_offsetX $ _axisLabel_textShadow x
  , _eChartAxisLabel_textShadowOffsetY = join $ fmap _shadow_offsetY $ _axisLabel_textShadow x
  }

toEChartAxisTick :: AxisTick -> EChartAxisTick
toEChartAxisTick x = EChartAxisTick
  { _eChartAxisTick_show = _axisTick_show x
  , _eChartAxisTick_alignWithLabel = _axisTick_alignWithLabel x
  , _eChartAxisTick_inside = _axisTick_inside x
  , _eChartAxisTick_length = _axisTick_length x
  , _eChartAxisTick_lineStyle = fmap toEChartLineStyle $ _axisTick_lineStyle x
  }

toEChartAxisLine :: AxisLine -> EChartAxisLine
toEChartAxisLine a = EChartAxisLine
  { _eChartAxisLine_onZero = _axisLine_onZero a
  , _eChartAxisLine_onZeroAxisIndex = _axisLine_onZeroAxisIndex a
  , _eChartAxisLine_show = _axisLine_show a
  , _eChartAxisLine_symbol = _axisLine_symbol a
  , _eChartAxisLine_symbolOffset = _axisLine_symbolOffset a
  , _eChartAxisLine_lineStyle = fmap toEChartLineStyle $ _axisLine_lineStyle a
  , _eChartAxisLine_symbolSize = _axisLine_symbolSize a
  }

toEChartLineStyle :: LineStyle -> EChartLineStyle
toEChartLineStyle x = EChartLineStyle
  { _eChartLineStyle_color = _lineStyle_color x
  , _eChartLineStyle_width = _lineStyle_width x
  , _eChartLineStyle_type = _lineStyle_type x
  , _eChartLineStyle_opacity = _lineStyle_opacity x
  , _eChartLineStyle_shadowColor = join $ fmap _shadow_color $ _lineStyle_shadow  x
  , _eChartLineStyle_shadowBlur = join $ fmap _shadow_blur $ _lineStyle_shadow x
  , _eChartLineStyle_shadowOffsetX = join $ fmap _shadow_offsetX $ _lineStyle_shadow x
  , _eChartLineStyle_shadowOffsetY = join $ fmap _shadow_offsetY $ _lineStyle_shadow x
  }
