{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
module Frontend where

import qualified Data.Text as T
import Obelisk.Frontend
import Obelisk.Route
import Reflex.Dom.Core

import Common.Api
import Common.Route
import Obelisk.Generated.Static

import JSDOM.Generated.Element hiding (Element)
import qualified JSDOM.Generated.Element as JSDOM
import JSDOM.Types
import Language.Javascript.JSaddle.Evaluate
import Language.Javascript.JSaddle.Object
import Data.Map (Map)
import qualified Data.Map as Map

data ECharts = ECharts { unECharts :: JSVal }

init :: JSDOM.Element -> JSM ECharts
init e = do
  f <- eval $ T.pack "(function(e) { return echarts['init'](e) })"
  arg <- toJSVal e
  ECharts <$> call f f [arg]

data Target = Target_Blank
            | Target_Self

data FontStyle = FontStyle_Normal
               | FontStyle_Italic
               | FontStyle_Oblique

data FontWeight = FontWeight_Normal
                | FontWeight_Bold
                | FontWeight_Bolder
                | FontWeight_Lighter
                | FontWeight_Numeric Int

data FontFamily = FontFamily_Serif
                | FontFamily_SansSerif
                | FontFamily_Monospace
                | FontFamily_Other Text

data Align = Align_Auto
           | Align_Left
           | Align_Center
           | Align_Right

data VerticalAlign = VerticalAlign_Auto
                   | VerticalAlign_Top
                   | VerticalAlign_Middle
                   | VerticalAlign_Bottom

data SizeValue = SizeValue_Auto
               | SizeValue_Percent Int
               | SizeValue_Numeric Int

data TextStyle = TextStyle
  { _textStyle_color :: Text
  , _textStyle_fontStyle :: FontStyle
  , _textStyle_fontWeight :: FontWeight
  , _textStyle_fontFamily :: FontFamily
  , _textStyle_fontSize :: Int
  , _textStyle_align :: Align
  , _textStyle_verticalAlign :: VerticalAlign
  , _textStyle_lineHeight :: Int
  , _textStyle_width :: SizeValue
  , _textStyle_textBorder :: Border
  , _textStyle_textShadow :: Shadow
  , _textStyle_rich :: Text
  }

data Border = Border
  { _border_color :: Text
  , _border_width :: Int
  , _border_radius :: (Int, Int, Int, Int)
  }

data Shadow = Shadow
  { _shadow_color :: Text
  , _shadow_blur :: Int
  , _shadow_offsetX :: Int
  , _shadow_offsetY :: Int
  }

data PosAlign = PosAlign_Auto
              | PosAlign_Pixel Int
              | PosAlign_Percent Int
              | PosAlign_Align Align

data Position = Position
  { _position_zlevel :: Int
  , _position_z :: Int
  , _position_left :: PosAlign
  , _position_top :: PosAlign
  , _position_right :: PosAlign
  , _position_bottom :: PosAlign
  }

data Size = Size
  { _size_width :: SizeValue
  , _size_height :: SizeValue
  }

data Orientation = Orientation_Horizontal
                 | Orientation_Vertical
                 | Orientation_Auto

data Title = Title
  { _title_show :: Bool
  , _title_text :: Text
  , _title_link :: Text
  , _title_target :: Target
  , _title_textStyle :: TextStyle
  , _title_subtext :: Text
  , _title_sublink :: Text
  , _title_subtarget :: Target
  , _title_subtextStyle :: TextStyle
  , _title_triggerEvent :: Bool
  , _title_padding :: (Int, Int, Int, Int)
  , _title_itemGap :: Int
  , _title_backgroundColor :: Text
  , _title_border :: Border
  , _title_shadow :: Shadow
  , _title_position :: Position
  }

data LegendType = LegendType_Plain
                | LegendType_Scroll

data Icon = Icon_Circle
          | Icon_Rect
          | Icon_RoundRect
          | Icon_Triangle
          | Icon_Diamond
          | Icon_Pin
          | Icon_Arrow
          | Icon_None
          | Icon_Image Text -- URL
          | Icon_DataURI Text
          | Icon_SVGPath Text

data LegendData = LegendData
  { _legendData_name :: Text
  , _legendData_icon :: Icon
  , _legendData_textStyle :: TextStyle
  }

data PageButtonPosition = PageButtonPosition_Start
                        | PageButtonPosition_End

data Legend = Legend
  { _legend_type :: LegendType
  , _legend_show :: Bool
  , _legend_position :: Position
  , _legend_size :: Size
  , _legend_orient :: Orientation
  , _legend_align :: Align
  , _legend_padding :: (Int, Int, Int, Int)
  , _legend_itemGap :: Int
  , _legend_itemWidth :: Int
  , _legend_itemHeight :: Int
  , _legend_symbolKeepAspect :: Bool
  , _legend_formatter :: Maybe Text
  , _legend_selectedMode :: Bool
  , _legend_inactiveColor :: Text
  , _legend_selected :: Map Text Bool
  , _legend_textStyle :: TextStyle
  -- , _legend_tooltip :: TODO
  , _legend_data :: Map Text LegendData
  , _legend_backgroundColor :: Text
  , _legend_border :: Border
  , _legend_shadow :: Shadow
  , _legend_scrollDataIndex :: Int
  , _legend_pageButtonItemGap :: Int
  , _legend_pageButtonGap :: Int
  , _legend_pageButtonPosition :: PageButtonPosition
  , _legend_pageFormatter :: Text
  -- , _legend_pageIcons ::  TODO
  , _legend_pageTextStyle :: TextStyle
  , _legend_animation :: Bool
  , _legend_animationDurationUpdate :: Int
  }

data Grid = Grid
  { _grid_show :: Bool
  , _grid_position :: Position
  , _grid_size :: Size
  , _grid_containLabel :: Bool
  , _grid_backgroundColor :: Text
  , _grid_border :: Border
  , _grid_shadow :: Shadow
  -- , _grid_tooltip :: TODO
  }

data XAxisPosition = XAxisPosition_Top
                   | XAxisPosition_Bottom

data XAxisType = XAxisType_Value
               | XAxisType_Category
               | XAxisType_Time
               | XAxisType_Log

data XAxis = XAxis
  { _xAxis_show :: Bool
  , _xAxis_gridIndex :: Int
  , _xAxis_position :: XAxisPosition
  , _xAxis_offset :: Int
  , _xAxis_type :: XAxisType
  , _xAxis_name :: Text
  , _xAxis_nameLocation :: Text -- TODO
  }
  -- , _xAxis_

-- setOption :: ECharts -> Option -> JSM ()
-- setOption e o = undefined

example :: JSDOM.Element -> JSM ()
example e = do
  f <- eval $ T.unlines
        [ "(function(e) {"
        , "var myChart = echarts['init'](e);"
        , "var option = { title: { text: 'Example' },"
        , "               tooltip: {},"
        , "               legend: { data: ['Sales'] },"
        , "               xAxis: { data: ["
        , "                 'shirt',"
        , "                 'cardigan',"
        , "                 'chiffon shirt',"
        , "                 'pants',"
        , "                 'heels',"
        , "                 'socks' ]"
        , "               },"
        , "               yAxis: {},"
        , "               series: [{"
        , "                 name: 'Sales',"
        , "                 type: 'bar',"
        , "                 data: [5, 20, 36, 10, 10, 20]"
        , "               }]"
        , "             };"
        , "myChart['setOption'](option);"
        , "})"
        ]
  arg <- toJSVal e
  call f f [arg]
  return ()

frontend :: Frontend (R FrontendRoute)
frontend = Frontend
  { _frontend_head = do
    el "title" $ text "Obelisk Minimal Example"
    elAttr "meta" ("charset" =: "utf-8") blank
    elAttr "script" ("type" =: "text/javascript" <> "src" =: static @"echarts.min.js") blank
  , _frontend_body = prerender blank echarts
  }

echarts
  :: ( DomBuilder t m
     , PostBuild t m
     , PerformEvent t m
     , MonadJSM (Performable m)
     , TriggerEvent t m
     , GhcjsDomSpace ~ DomBuilderSpace m
     )
  => m ()
echarts = el "main" $ do
  (e, _) <- elAttr' "div" ("style" =: "width:600px;height:400px;") blank
  p <- getPostBuild
  performEvent_ $ ffor p $ \_ -> liftJSM $ example $ _element_raw e
  return ()

