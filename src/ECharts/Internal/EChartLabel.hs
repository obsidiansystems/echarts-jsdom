{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module ECharts.Internal.EChartLabel where

import qualified Data.Aeson as Aeson
import Data.Text (Text)
import GHC.Generics (Generic)

import ECharts.Types

data EChartLabel = EChartLabel
  { _eChartLabel_show :: Maybe Bool
  , _eChartLabel_position :: Maybe Position
  , _eChartLabel_distance :: Maybe Double
  , _eChartLabel_rotate :: Maybe Double
  , _eChartLabel_offset :: Maybe (Double, Double)
  , _eChartLabel_formatter :: Maybe Aeson.Value
  , _eChartLabel_color :: Maybe Text
  , _eChartLabel_fontStyle :: Maybe FontStyle
  , _eChartLabel_fontWeight :: Maybe FontWeight
  , _eChartLabel_fontFamily :: Maybe FontFamily
  , _eChartLabel_fontSize :: Maybe Int
  , _eChartLabel_align :: Maybe Align
  , _eChartLabel_verticalAlign :: Maybe VerticalAlign
  , _eChartLabel_lineHeight :: Maybe Int
  , _eChartLabel_backgroundColor :: Maybe Text
  , _eChartLabel_padding :: Maybe [Double]
  , _eChartLabel_borderColor :: Maybe Text
  , _eChartLabel_borderWidth :: Maybe Int
  , _eChartLabel_borderRadius :: Maybe (Int, Int, Int, Int)
  , _eChartLabel_shadowColor :: Maybe Text
  , _eChartLabel_shadowBlur :: Maybe Int
  , _eChartLabel_shadowOffsetX :: Maybe Int
  , _eChartLabel_shadowOffsetY :: Maybe Int
  , _eChartLabel_width :: Maybe SN
  , _eChartLabel_height :: Maybe SN
  , _eChartLabel_textBorderColor :: Maybe Text
  , _eChartLabel_textBorderWidth :: Maybe Int
  , _eChartLabel_textShadowColor :: Maybe Text
  , _eChartLabel_textShadowBlur :: Maybe Int
  , _eChartLabel_textShadowOffsetX :: Maybe Int
  , _eChartLabel_textShadowOffsetY :: Maybe Int
  , _eChartLabel_rich :: Maybe Aeson.Value
  -- This is not mentioned in docs, but is used examples (tooltip.axisPointer.label)
  , _eChartLabel_textStyle :: Maybe TextStyle
  }
  deriving (Generic)

toEChartLabel :: Label -> EChartLabel
toEChartLabel v = EChartLabel
  { _eChartLabel_show = _label_show v
  , _eChartLabel_position = _label_position v
  , _eChartLabel_distance = _label_distance v
  , _eChartLabel_rotate = _label_rotate v
  , _eChartLabel_offset = _label_offset v
  , _eChartLabel_formatter = _label_formatter v
  , _eChartLabel_color = _label_color v
  , _eChartLabel_fontStyle = _font_style =<< _label_font v
  , _eChartLabel_fontWeight = _font_weight =<< _label_font v
  , _eChartLabel_fontFamily = _font_family =<< _label_font v
  , _eChartLabel_fontSize = _font_size =<< _label_font v
  , _eChartLabel_align = _label_align v
  , _eChartLabel_verticalAlign = _label_verticalAlign v
  , _eChartLabel_lineHeight = _label_lineHeight v
  , _eChartLabel_backgroundColor = _label_backgroundColor v
  , _eChartLabel_padding = _label_padding v
  , _eChartLabel_borderColor = _border_color =<< _label_border v
  , _eChartLabel_borderWidth = _border_width =<< _label_border v
  , _eChartLabel_borderRadius = _border_radius =<< _label_border v
  , _eChartLabel_shadowColor = _shadow_color =<< _label_shadow v
  , _eChartLabel_shadowBlur = _shadow_blur =<< _label_shadow v
  , _eChartLabel_shadowOffsetX = _shadow_offsetX =<< _label_shadow v
  , _eChartLabel_shadowOffsetY = _shadow_offsetY =<< _label_shadow v
  , _eChartLabel_width = sizeValueToSN <$> (_size_width =<< _label_size v)
  , _eChartLabel_height = sizeValueToSN <$> (_size_height =<< _label_size v)
  , _eChartLabel_textBorderColor = _border_color =<< _label_textBorder v
  , _eChartLabel_textBorderWidth = _border_width =<< _label_textBorder v
  , _eChartLabel_textShadowColor = _shadow_color =<< _label_textShadow v
  , _eChartLabel_textShadowBlur = _shadow_blur =<< _label_textShadow v
  , _eChartLabel_textShadowOffsetX = _shadow_offsetX =<< _label_textShadow v
  , _eChartLabel_textShadowOffsetY = _shadow_offsetY =<< _label_textShadow v
  , _eChartLabel_rich = _label_rich v
  , _eChartLabel_textStyle = _label_textStyle v
  }
