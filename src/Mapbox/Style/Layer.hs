{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
module Mapbox.Style.Layer (
  Anchor (..)
, Layer (..)
, Transition (..)
, Transitionable (..)
, Pixels
, Ems
, Factor
, Degrees
, SourceRef (..)
, SpriteId (..)
, SpriteMap
, Sprite (..)
, Property (..)
, ZoomStop (..)
, PropStop (..)
, ZoomPropStop (..)
, Stops (..)
, ColorSpace (..)
, Visibility (..)
, LineCap (..)
, LineJoin (..)
, Padding (..)
, Translate
, Offset
, XY (..)
, SymbolPlacement (..)
, Alignment (..)
, TextFit (..)
, Justify (..)
, BoxAnchor (..)
, TextTransform (..)
, FontList (..)
, DashArray (..)

, tp
, background
, fill
, line
, symbol
, rasterLayer
, circle
, fillExtrusion
, heatmap
, hillshade

, derefLayers
) where

import Mapbox.Style.Common (failT, prop)
import Mapbox.Style.Expression ( Expr(Lit), IsValue(..), parseExpr, parseArray
                               , fromLiteralArr, toLiteralArr
                               )
import Mapbox.Style.Types
import Data.Aeson
import Data.Aeson.TH (deriveJSON)
import Data.Aeson.Types (Pair, Parser)
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V
import GHC.Exts (IsList(..))
import Protolude hiding (filter,join)



data Layer v
  = Background
    { id          :: Text
    , visibility  :: Maybe Visibility
    , interactive :: Maybe Bool
    , metadata    :: Maybe (StrMap Value)
    , minzoom     :: Maybe Zoom
    , maxzoom     :: Maybe Zoom
    , filter      :: Maybe (Expr Bool)

    , color       :: Maybe (Transitionable (Property Color))
    , pattern     :: Maybe (Transitionable (Property SpriteId))
    , opacity     :: Maybe (Transitionable (Property Number))
    }
  | Fill
    { id          :: Text
    , visibility  :: Maybe Visibility
    , interactive :: Maybe Bool
    , metadata    :: Maybe (StrMap Value)
    , minzoom     :: Maybe Zoom
    , maxzoom     :: Maybe Zoom
    , filter      :: Maybe (Expr Bool)
    , source      :: SourceRef

    , antialias   :: Maybe (Property Bool)
    , opacity     :: Maybe (Transitionable (Property Number))
    , color       :: Maybe (Transitionable (Property Color))
    , outlineColor:: Maybe (Transitionable (Property Color))
    , translate   :: Maybe (Transitionable (Property (XY Translate)))
    , translateAnchor :: Maybe (Property Anchor)
    , pattern     :: Maybe (Transitionable (Property SpriteId))
    }
  | Line
    { id          :: Text
    , visibility  :: Maybe Visibility
    , interactive :: Maybe Bool
    , metadata    :: Maybe (StrMap Value)
    , minzoom     :: Maybe Zoom
    , maxzoom     :: Maybe Zoom
    , filter      :: Maybe (Expr Bool)
    , source      :: SourceRef

    , cap         :: Maybe (Property LineCap)
    , join        :: Maybe (Property LineJoin)
    , miterLimit  :: Maybe (Property Pixels)
    , roundLimit  :: Maybe (Property Pixels)
    , opacity     :: Maybe (Transitionable (Property Number))
    , color       :: Maybe (Transitionable (Property Color))
    , translate   :: Maybe (Transitionable (Property (XY Translate)))
    , translateAnchor :: Maybe (Property Anchor)
    , width  :: Maybe (Transitionable (Property Pixels))
    , gapWidth  :: Maybe (Transitionable (Property Pixels))
    , lineOffset  :: Maybe (Transitionable (Property Pixels))
    , blur  :: Maybe (Transitionable (Property Number))
    , dashArray  :: Maybe (Transitionable (Property DashArray))
    , pattern     :: Maybe (Transitionable (Property SpriteId))
    }
  | Symbol
    { id          :: Text
    , visibility  :: Maybe Visibility
    , interactive :: Maybe Bool
    , metadata    :: Maybe (StrMap Value)
    , minzoom     :: Maybe Zoom
    , maxzoom     :: Maybe Zoom
    , filter      :: Maybe (Expr Bool)
    , source      :: SourceRef

    , symPlacement    :: Maybe (Property SymbolPlacement)
    , symSpacing      :: Maybe (Property Pixels)
    , symAvoidEdges   :: Maybe (Property Bool)
    , iconAllowOverlap :: Maybe (Property Bool)
    , iconIgnorePlacement :: Maybe (Property Bool)
    , iconOptional        :: Maybe (Property Bool)
    , iconRotationAlignment :: Maybe (Property Alignment)
    , iconSize              :: Maybe (Property Factor)
    , iconTextFit           :: Maybe (Property TextFit)
    , iconTextFitPadding    :: Maybe (Property Padding)
    , iconImage             :: Maybe (Property SpriteId)
    , iconRotate            :: Maybe (Property Degrees)
    , iconPadding          :: Maybe (Property Pixels)
    , iconKeepUpright     :: Maybe (Property Bool)
    , iconOffset          :: Maybe (Property (XY Offset))
    , iconAnchor          :: Maybe (Property BoxAnchor)
    , iconPitchAlignment :: Maybe (Property Alignment)
    , textPitchAlignment :: Maybe (Property Alignment)
    , textRotationAlignment :: Maybe (Property Alignment)
    , textField             :: Maybe (Property Text)
    , textFont             :: Maybe (Property FontList)
    , textSize             :: Maybe (Property Pixels)
    , textMaxWidth             :: Maybe (Property Ems)
    , textLineHeight             :: Maybe (Property Ems)
    , textLetterSpacing             :: Maybe (Property Ems)
    , textJustify             :: Maybe (Property Justify)
    , textAnchor             :: Maybe (Property BoxAnchor)
    , textMaxAngle             :: Maybe (Property Degrees)
    , textRotate             :: Maybe (Property Degrees)
    , textPadding             :: Maybe (Property Pixels)
    , textKeepUpright         :: Maybe (Property Bool)
    , textTransform         :: Maybe (Property TextTransform)
    , textOffset         :: Maybe (Property (XY Offset))
    , textAllowOverlap         :: Maybe (Property Bool)
    , textIgnorePlacement         :: Maybe (Property Bool)
    , textOptional         :: Maybe (Property Bool)
    , iconOpacity     :: Maybe (Transitionable (Property Number))
    , iconColor       :: Maybe (Transitionable (Property Color))
    , iconHaloColor       :: Maybe (Transitionable (Property Color))
    , iconHaloWidth       :: Maybe (Transitionable (Property Pixels))
    , iconHaloBlur       :: Maybe (Transitionable (Property Number))
    , iconTranslate       :: Maybe (Transitionable (Property (XY Translate)))
    , iconTranslateAnchor         :: Maybe (Property Anchor)
    , textOpacity :: Maybe (Transitionable (Property Number))
    , textColor       :: Maybe (Transitionable (Property Color))
    , textHaloColor       :: Maybe (Transitionable (Property Color))
    , textHaloWidth       :: Maybe (Transitionable (Property Pixels))
    , textHaloBlur       :: Maybe (Transitionable (Property Number))
    , textTranslate       :: Maybe (Transitionable (Property (XY Translate)))
    , textTranslateAnchor         :: Maybe (Property Anchor)
    }
  | RasterLayer
    { id          :: Text
    , visibility  :: Maybe Visibility
    , interactive :: Maybe Bool
    , metadata    :: Maybe (StrMap Value)
    , minzoom     :: Maybe Zoom
    , maxzoom     :: Maybe Zoom
    , filter      :: Maybe (Expr Bool)
    , source      :: SourceRef

    , opacity     :: Maybe (Transitionable (Property Number))
    , hueRotate     :: Maybe (Transitionable (Property Degrees))
    , brightnessMin     :: Maybe (Transitionable (Property Number))
    , brightnessMax     :: Maybe (Transitionable (Property Number))
    , saturation     :: Maybe (Transitionable (Property Number)) --TODO use a newtype for [-1,-1]
    , contrast     :: Maybe (Transitionable (Property Number)) --TODO ditto
    , fadeDuration     :: Maybe (Property Milliseconds)
    }
  | Circle
    { id          :: Text
    , visibility  :: Maybe Visibility
    , interactive :: Maybe Bool
    , metadata    :: Maybe (StrMap Value)
    , minzoom     :: Maybe Zoom
    , maxzoom     :: Maybe Zoom
    , filter      :: Maybe (Expr Bool)
    , source      :: SourceRef

    , radius     :: Maybe (Transitionable (Property Pixels))
    , color     :: Maybe (Transitionable (Property Color))
    , blur     :: Maybe (Transitionable (Property Number))
    , opacity     :: Maybe (Transitionable (Property Number))
    , translate       :: Maybe (Transitionable (Property (XY Translate)))
    , translateAnchor         :: Maybe (Property Anchor)
    , pitchScale         :: Maybe (Property Anchor)
    , pitchAlignment         :: Maybe (Property Anchor)
    , strokeWidth     :: Maybe (Transitionable (Property Pixels))
    , strokeColor     :: Maybe (Transitionable (Property Color))
    , strokeOpacity     :: Maybe (Transitionable (Property Number))
    }
  | FillExtrusion
    { id          :: Text
    , visibility  :: Maybe Visibility
    , interactive :: Maybe Bool
    , metadata    :: Maybe (StrMap Value)
    , minzoom     :: Maybe Zoom
    , maxzoom     :: Maybe Zoom
    , filter      :: Maybe (Expr Bool)
    , source      :: SourceRef

    , opacity     :: Maybe (Transitionable (Property Number))
    , color     :: Maybe (Transitionable (Property Color))
    , translate       :: Maybe (Transitionable (Property (XY Translate)))
    , translateAnchor         :: Maybe (Property Anchor)
    , pattern     :: Maybe (Transitionable (Property SpriteId))
    , height     :: Maybe (Transitionable (Property Number))
    , base     :: Maybe (Transitionable (Property Number))
    }
  | Heatmap
    { id          :: Text
    , visibility  :: Maybe Visibility
    , interactive :: Maybe Bool
    , metadata    :: Maybe (StrMap Value)
    , minzoom     :: Maybe Zoom
    , maxzoom     :: Maybe Zoom
    , filter      :: Maybe (Expr Bool)
    , source      :: SourceRef

    , radius     :: Maybe (Transitionable (Property Pixels))
    , weight     :: Maybe (Transitionable (Property Number))
    , intensity     :: Maybe (Transitionable (Property Number))
    , hmColor   :: Maybe (Property Color)
    , opacity     :: Maybe (Transitionable (Property Number))
    }
  | Hillshade
    { id          :: Text
    , visibility  :: Maybe Visibility
    , interactive :: Maybe Bool
    , metadata    :: Maybe (StrMap Value)
    , minzoom     :: Maybe Zoom
    , maxzoom     :: Maybe Zoom
    , filter      :: Maybe (Expr Bool)
    , source      :: SourceRef

    , illuminationDirection :: Maybe (Property Degrees)
    , illuminationAnchor :: Maybe (Property Anchor)
    , exageration     :: Maybe (Transitionable (Property Number))
    , shadowColor     :: Maybe (Transitionable (Property Color))
    , highlightColor     :: Maybe (Transitionable (Property Color))
    , accentColor     :: Maybe (Transitionable (Property Color))
    }
  | VendorLayer v
  deriving (Eq, Show, Generic)

background :: Text -> Layer v
background id = Background
  { id
  , visibility  = Nothing
  , interactive = Nothing
  , metadata    = Nothing
  , minzoom     = Nothing
  , maxzoom     = Nothing
  , filter      = Nothing

  , color       = Nothing
  , pattern     = Nothing
  , opacity     = Nothing
  }

fill :: Text -> SourceRef -> Layer v
fill id source = Fill
  { id
  , visibility  = Nothing
  , interactive = Nothing
  , metadata    = Nothing
  , minzoom     = Nothing
  , maxzoom     = Nothing
  , filter      = Nothing
  , source

  , antialias       = Nothing
  , opacity         = Nothing
  , color           = Nothing
  , outlineColor    = Nothing
  , translate       = Nothing
  , translateAnchor = Nothing
  , pattern         = Nothing
  }

line :: Text -> SourceRef -> Layer v
line id source = Line
  { id
  , visibility  = Nothing
  , interactive = Nothing
  , metadata    = Nothing
  , minzoom     = Nothing
  , maxzoom     = Nothing
  , filter      = Nothing
  , source

  , cap         = Nothing
  , join        = Nothing
  , miterLimit  = Nothing
  , roundLimit  = Nothing
  , opacity     = Nothing
  , color       = Nothing
  , translate   = Nothing
  , translateAnchor = Nothing
  , width  = Nothing
  , gapWidth  = Nothing
  , lineOffset  = Nothing
  , blur  = Nothing
  , dashArray  = Nothing
  , pattern     = Nothing
  }

symbol :: Text -> SourceRef -> Layer v
symbol id source = Symbol
  { id
  , visibility  = Nothing
  , interactive = Nothing
  , metadata    = Nothing
  , minzoom     = Nothing
  , maxzoom     = Nothing
  , filter      = Nothing
  , source

  , symPlacement    = Nothing
  , symSpacing      = Nothing
  , symAvoidEdges   = Nothing
  , iconAllowOverlap = Nothing
  , iconIgnorePlacement = Nothing
  , iconOptional        = Nothing
  , iconRotationAlignment = Nothing
  , iconSize              = Nothing
  , iconTextFit           = Nothing
  , iconTextFitPadding    = Nothing
  , iconImage             = Nothing
  , iconRotate            = Nothing
  , iconPadding          = Nothing
  , iconKeepUpright     = Nothing
  , iconOffset          = Nothing
  , iconAnchor          = Nothing
  , iconPitchAlignment = Nothing
  , textPitchAlignment = Nothing
  , textRotationAlignment = Nothing
  , textField             = Nothing
  , textFont             = Nothing
  , textSize             = Nothing
  , textMaxWidth         = Nothing
  , textLineHeight       = Nothing
  , textLetterSpacing    = Nothing
  , textJustify          = Nothing
  , textAnchor           = Nothing
  , textMaxAngle         = Nothing
  , textRotate           = Nothing
  , textPadding          = Nothing
  , textKeepUpright      = Nothing
  , textTransform        = Nothing
  , textOffset         = Nothing
  , textAllowOverlap     = Nothing
  , textIgnorePlacement  = Nothing
  , textOptional         = Nothing
  , iconOpacity     = Nothing
  , iconColor       = Nothing
  , iconHaloColor       = Nothing
  , iconHaloWidth       = Nothing
  , iconHaloBlur       = Nothing
  , iconTranslate       = Nothing
  , iconTranslateAnchor = Nothing
  , textOpacity = Nothing
  , textColor     = Nothing
  , textHaloColor   = Nothing
  , textHaloWidth    = Nothing
  , textHaloBlur      = Nothing
  , textTranslate      = Nothing
  , textTranslateAnchor  = Nothing
  }

rasterLayer :: Text -> SourceRef -> Layer v
rasterLayer id source = RasterLayer
  { id
  , visibility  = Nothing
  , interactive = Nothing
  , metadata    = Nothing
  , minzoom     = Nothing
  , maxzoom     = Nothing
  , filter      = Nothing
  , source
  , opacity     = Nothing
  , hueRotate     = Nothing
  , brightnessMin  = Nothing
  , brightnessMax  = Nothing
  , saturation     = Nothing
  , contrast     = Nothing
  , fadeDuration  = Nothing
  }

circle :: Text -> SourceRef -> Layer v
circle id source = Circle
  { id
  , visibility  = Nothing
  , interactive = Nothing
  , metadata    = Nothing
  , minzoom     = Nothing
  , maxzoom     = Nothing
  , filter      = Nothing
  , source

  , radius     = Nothing
  , color     = Nothing
  , blur     = Nothing
  , opacity     = Nothing
  , translate       = Nothing
  , translateAnchor  = Nothing
  , pitchScale      = Nothing
  , pitchAlignment  = Nothing
  , strokeWidth     = Nothing
  , strokeColor     = Nothing
  , strokeOpacity   = Nothing
  }

fillExtrusion :: Text -> SourceRef -> Layer v
fillExtrusion id source = FillExtrusion
  { id
  , visibility  = Nothing
  , interactive = Nothing
  , metadata    = Nothing
  , minzoom     = Nothing
  , maxzoom     = Nothing
  , filter      = Nothing
  , source

  , opacity     = Nothing
  , color     = Nothing
  , translate     = Nothing
  , translateAnchor = Nothing
  , pattern   = Nothing
  , height    = Nothing
  , base    = Nothing
  }

heatmap :: Text -> SourceRef -> Layer v
heatmap id source = Heatmap
  { id
  , visibility  = Nothing
  , interactive = Nothing
  , metadata    = Nothing
  , minzoom     = Nothing
  , maxzoom     = Nothing
  , filter      = Nothing
  , source

  , radius     = Nothing
  , weight     = Nothing
  , intensity   = Nothing
  , hmColor   = Nothing
  , opacity     = Nothing
  }

hillshade :: Text -> SourceRef -> Layer v
hillshade id source = Hillshade
  { id
  , visibility  = Nothing
  , interactive = Nothing
  , metadata    = Nothing
  , minzoom     = Nothing
  , maxzoom     = Nothing
  , filter      = Nothing
  , source

  , illuminationDirection = Nothing
  , illuminationAnchor = Nothing
  , exageration     = Nothing
  , shadowColor     = Nothing
  , highlightColor     = Nothing
  , accentColor     = Nothing
  }

mapProp :: Text -> [[Pair]] -> Maybe Pair
mapProp k pss = case concat pss of
  [] -> Nothing
  ps -> Just (k .= object ps)

paintProps, layoutProps :: [[Pair]] -> Maybe Pair
paintProps = mapProp "paint"
layoutProps = mapProp "layout"


instance ToJSON v => ToJSON (Layer v) where
  toJSON (Background {..}) = object $ catMaybes
    [ Just ("id" .= id), Just ("type","background")
    , prop "metadata" metadata
    , prop "minzoom" minzoom
    , prop "maxzoom" maxzoom
    , prop "filter" filter
    , prop "interactive" interactive
    , layoutProps
      [ propL "visibility" visibility
      ]
    , paintProps
      [ transProp "background-color" color
      , transProp "background-pattern" pattern
      , transProp "background-opacity" opacity
      ]
    ]
  toJSON (Fill {..}) = object $ sourceRefPairs source <> catMaybes
    [ Just ("id" .= id), Just ("type","fill")
    , prop "metadata" metadata
    , prop "minzoom" minzoom
    , prop "maxzoom" maxzoom
    , prop "filter" filter
    , prop "interactive" interactive
    , layoutProps
      [ propL "visibility" visibility
      ]
    , paintProps
      [ transProp "fill-color" color
      , propL "fill-antialias" antialias
      , transProp "fill-opacity" opacity
      , transProp "fill-outline-color" outlineColor
      , transProp "fill-translate" translate
      , propL     "fill-translate-anchor" translateAnchor
      , transProp "fill-pattern" pattern
      ]
    ]
  toJSON (Line {..}) = object $ sourceRefPairs source <> catMaybes
    [ Just ("id" .= id), Just ("type","line")
    , prop "metadata" metadata
    , prop "minzoom" minzoom
    , prop "maxzoom" maxzoom
    , prop "filter" filter
    , prop "interactive" interactive
    , layoutProps
      [ propL "visibility" visibility
      , propL "line-cap" cap
      , propL "line-join" join
      , propL "line-miter-limit" miterLimit
      , propL "line-round-limit" roundLimit
      ]
    , paintProps
      [ transProp "line-opacity" opacity
      , transProp "line-color" color
      , transProp "line-translate" translate
      , propL     "line-translate-anchor" translateAnchor
      , transProp "line-width" width
      , transProp "line-gap-width" gapWidth
      , transProp "line-offset" lineOffset
      , transProp "line-blur" blur
      , transProp "line-dasharray" dashArray
      , transProp "line-pattern" pattern
      ]
    ]
  toJSON (Symbol {..}) = object $ sourceRefPairs source <> catMaybes
    [ Just ("id" .= id), Just ("type","symbol")
    , prop "metadata" metadata
    , prop "minzoom" minzoom
    , prop "maxzoom" maxzoom
    , prop "filter" filter
    , prop "interactive" interactive
    , layoutProps
      [ propL     "visibility" visibility
      , propL     "symbol-placement" symPlacement
      , propL     "symbol-spacing" symSpacing
      , propL     "symbol-avoid-edges" symAvoidEdges
      , propL     "icon-allow-overlap" iconAllowOverlap
      , propL     "icon-ignore-placement" iconIgnorePlacement
      , propL     "icon-optional" iconOptional
      , propL     "icon-rotation-alignment" iconRotationAlignment
      , propL     "icon-size" iconSize
      , propL     "icon-text-fit" iconTextFit
      , propL     "icon-text-fit-padding" iconTextFitPadding
      , propL     "icon-image" iconImage
      , propL     "icon-rotate" iconRotate
      , propL     "icon-padding" iconPadding
      , propL     "icon-keep-upright" iconKeepUpright
      , propL     "icon-offset" iconOffset
      , propL     "icon-anchor" iconAnchor
      , propL     "icon-pitch-alignment" iconPitchAlignment
      , propL     "text-pitch-alignment" textPitchAlignment
      , propL     "text-rotation-alignment" textRotationAlignment
      , propL     "text-field" textField
      , propL     "text-font" textFont
      , propL     "text-size" textSize
      , propL     "text-max-width" textMaxWidth
      , propL     "text-line-height" textLineHeight
      , propL     "text-letter-spacing" textLetterSpacing
      , propL     "text-justify" textJustify
      , propL     "text-anchor" textAnchor
      , propL     "text-max-angle" textMaxAngle
      , propL     "text-rotate" textRotate
      , propL     "text-padding" textPadding
      , propL     "text-keep-upright" textKeepUpright
      , propL     "text-transform" textTransform
      , propL     "text-offset" textOffset
      , propL     "text-allow-overlap" textAllowOverlap
      , propL     "text-ignore-placement" textIgnorePlacement
      , propL     "text-optional" textOptional
      ]
    , paintProps
      [ transProp "icon-opacity" iconOpacity
      , transProp "icon-color" iconColor
      , transProp "icon-halo-color" iconHaloColor
      , transProp "icon-halo-width" iconHaloWidth
      , transProp "icon-halo-blur" iconHaloBlur
      , transProp "icon-translate" iconTranslate
      , propL     "icon-translate-anchor" iconTranslateAnchor
      , transProp "text-opacity" textOpacity
      , transProp "text-color" textColor
      , transProp "text-halo-color" textHaloColor
      , transProp "text-halo-width" textHaloWidth
      , transProp "text-halo-blur" textHaloBlur
      , transProp "text-translate" textTranslate
      , propL     "text-translate-anchor" textTranslateAnchor
      ]
    ]
  toJSON (RasterLayer {..}) = object $ sourceRefPairs source <> catMaybes
    [ Just ("id" .= id), Just ("type","raster")
    , prop "metadata" metadata
    , prop "minzoom" minzoom
    , prop "maxzoom" maxzoom
    , prop "filter" filter
    , prop "interactive" interactive
    , layoutProps
      [ propL "visibility" visibility
      ]
    , paintProps
      [ transProp "raster-opacity" opacity
      , transProp "raster-hue-rotate" hueRotate
      , transProp "raster-brightness-min" brightnessMin
      , transProp "raster-brightness-max" brightnessMax
      , transProp "raster-saturation" saturation
      , transProp "raster-contrast" contrast
      , propL     "raster-fade-duration" fadeDuration
      ]
    ]
  toJSON (Circle {..}) = object $ sourceRefPairs source <> catMaybes
    [ Just ("id" .= id), Just ("type","circle")
    , prop "metadata" metadata
    , prop "minzoom" minzoom
    , prop "maxzoom" maxzoom
    , prop "filter" filter
    , prop "interactive" interactive
    , layoutProps
      [ propL "visibility" visibility
      ]
    , paintProps
      [ transProp "circle-radius" radius
      , transProp "circle-color" color
      , transProp "circle-blur" blur
      , transProp "circle-opacity" opacity
      , transProp "circle-translate" translate
      , propL     "circle-translate-anchor" translateAnchor
      , propL     "circle-pitch-scale" pitchScale
      , propL     "circle-pitch-alignment" pitchAlignment
      , transProp "circle-stroke-width" strokeWidth
      , transProp "circle-stroke-color" strokeColor
      , transProp "circle-stroke-opacity" strokeOpacity
      ]
    ]
  toJSON (FillExtrusion {..}) = object $ sourceRefPairs source <> catMaybes
    [ Just ("id" .= id), Just ("type","fill-extrusion")
    , prop "metadata" metadata
    , prop "minzoom" minzoom
    , prop "maxzoom" maxzoom
    , prop "filter" filter
    , prop "interactive" interactive
    , layoutProps
      [ propL "visibility" visibility
      ]
    , paintProps
      [ transProp "fill-extrusion-opacity" opacity
      , transProp "fill-extrusion-color" color
      , transProp "fill-extrusion-translate" translate
      , propL     "fill-extrusion-translate-anchor" translateAnchor
      , transProp "fill-extrusion-pattern" pattern
      , transProp "fill-extrusion-height" height
      , transProp "fill-extrusion-base" base
      ]
    ]
  toJSON (Heatmap {..}) = object $ sourceRefPairs source <> catMaybes
    [ Just ("id" .= id), Just ("type","heatmap")
    , prop "metadata" metadata
    , prop "minzoom" minzoom
    , prop "maxzoom" maxzoom
    , prop "filter" filter
    , prop "interactive" interactive
    , layoutProps
      [ propL "visibility" visibility
      ]
    , paintProps
      [ transProp "heatmap-radius" radius
      , transProp "heatmap-weight" weight
      , transProp "heatmap-intensity" intensity
      , propL     "heatmap-color" hmColor
      , transProp "heatmap-opacity" opacity
      ]
    ]
  toJSON (Hillshade {..}) = object $ sourceRefPairs source <> catMaybes
    [ Just ("id" .= id), Just ("type","hillshade")
    , prop "metadata" metadata
    , prop "minzoom" minzoom
    , prop "maxzoom" maxzoom
    , prop "filter" filter
    , prop "interactive" interactive
    , layoutProps
      [ propL "visibility" visibility
      ]
    , paintProps
      [ propL     "hillshade-illumination-direction" illuminationDirection
      , propL     "hillshade-illumination-anchor" illuminationAnchor
      , transProp "hillshade-exaggeration" exageration
      , transProp "hillshade-shadow-color" shadowColor
      , transProp "hillshade-highlight-color" highlightColor
      , transProp "hillshade-accent-color" accentColor
      ]
    ]
  toJSON (VendorLayer v) = toJSON v

propL
  :: ToJSON o
  => Text -> Maybe o
  -> [Pair]
propL t = maybeToList . fmap (t.=)

transProp
  :: IsValue o
  => Text -> Maybe (Transitionable (Property o))
  -> [Pair]
transProp _ Nothing = []
transProp t (Just (T o Nothing)) = [t .= o]
transProp t (Just (T o (Just tr))) =
  [ t .= o
  , (t <> "-transition") .= tr
  ]

instance FromJSON v => FromJSON (Layer v) where
  parseJSON ob@(Object m) = do
    id <- m .: "id"
    metadata <- m .:? "metadata"
    minzoom <- m .:? "minzoom"
    maxzoom <- m .:? "maxzoom"
    filter <- m .:? "filter"
    interactive <- m .:? "interactive"

    paint <- m .:? "paint" .!= mempty
    layout <- m .:? "layout" .!= mempty
    visibility <- layout .:? "visibility"
    type_ <- m .: "type"
    case type_ :: Text of
      "background" -> do
        color <- getTransitionableProp paint "background-color"
        pattern <- getTransitionableProp paint "background-pattern"
        opacity <- getTransitionableProp paint  "background-opacity"
        pure Background{..}
      "fill" -> do
        source <- decodeSourceRef m
        antialias <- getProp paint "fill-antialias"
        opacity <- getTransitionableProp paint  "fill-opacity"
        color <- getTransitionableProp paint "fill-color"
        outlineColor <- getTransitionableProp paint "fill-outline-color"
        translate <- getTransitionableProp paint "fill-translate"
        translateAnchor <- getProp paint "fill-translate-anchor"
        pattern <- getTransitionableProp paint "fill-pattern"
        pure Fill{..}
      "line" -> do
        source <- decodeSourceRef m
        cap <- getProp layout "line-cap"
        join <- getProp layout "line-join"
        miterLimit <- getProp layout "line-miter-limit"
        roundLimit <- getProp layout "line-round-limit"
        opacity <- getTransitionableProp paint  "line-opacity"
        color <- getTransitionableProp paint "line-color"
        translate <- getTransitionableProp paint "line-translate"
        translateAnchor <- getProp paint "line-translate-anchor"
        width <- getTransitionableProp paint  "line-width"
        gapWidth <- getTransitionableProp paint  "line-gap-width"
        lineOffset <- getTransitionableProp paint  "line-offset"
        blur <- getTransitionableProp paint  "line-blur"
        dashArray <- getTransitionableProp paint  "line-dasharray"
        pattern <- getTransitionableProp paint  "line-pattern"
        pure Line {..}
      "symbol" -> do
        source <- decodeSourceRef m
        symPlacement <- getProp layout "symbol-placement"
        symSpacing   <- getProp layout "symbol-spacing"
        symAvoidEdges <- getProp layout "symbol-avoid-edges"
        iconAllowOverlap <- getProp layout "icon-allow-overlap"
        iconIgnorePlacement <- getProp layout "icon-ignore-placement"
        iconOptional <- getProp layout "icon-optional"
        iconRotationAlignment <- getProp layout "icon-rotation-alignment"
        iconSize <- getProp layout "icon-size"
        iconTextFit <- getProp layout "icon-text-fit"
        iconTextFitPadding <- getProp layout "icon-text-fit-padding"
        iconImage <- getProp layout "icon-image"
        iconRotate <- getProp layout "icon-rotate"
        iconPadding <- getProp layout "icon-padding"
        iconKeepUpright <- getProp layout "icon-keep-upright"
        iconOffset <- getProp layout "icon-offset"
        iconAnchor <- getProp layout "icon-anchor"
        iconPitchAlignment <- getProp layout "icon-pitch-alignment"
        textPitchAlignment <- getProp layout "text-pitch-alignment"
        textRotationAlignment <- getProp layout "text-rotation-alignment"
        textField <- getProp layout "text-field"
        textFont <- getProp layout "text-font"
        textSize <- getProp layout "text-size"
        textMaxWidth <- getProp layout "text-max-width"
        textLineHeight <- getProp layout "text-line-height"
        textLetterSpacing <- getProp layout "text-letter-spacing"
        textJustify <- getProp layout "text-justify"
        textAnchor <- getProp layout "text-anchor"
        textMaxAngle <- getProp layout "text-max-angle"
        textRotate <- getProp layout "text-rotate"
        textPadding <- getProp layout "text-padding"
        textKeepUpright <- getProp layout "text-keep-upright"
        textTransform <- getProp layout "text-transform"
        textOffset <- getProp layout "text-offset"
        textAllowOverlap <- getProp layout "text-allow-overlap"
        textIgnorePlacement <- getProp layout "text-ignore-placement"
        textOptional <- getProp layout "text-optional"
        iconOpacity <- getTransitionableProp paint "icon-opacity"
        iconColor <- getTransitionableProp paint "icon-color"
        iconHaloColor <- getTransitionableProp paint "icon-halo-color"
        iconHaloWidth <- getTransitionableProp paint "icon-halo-width"
        iconHaloBlur <- getTransitionableProp paint "icon-halo-blur"
        iconTranslate <- getTransitionableProp paint "icon-translate"
        iconTranslateAnchor <- getProp paint "icon-translate-anchor"
        textOpacity <- getTransitionableProp paint "text-opacity"
        textColor <- getTransitionableProp paint "text-color"
        textHaloColor <- getTransitionableProp paint "text-halo-color"
        textHaloWidth <- getTransitionableProp paint "text-halo-width"
        textHaloBlur <- getTransitionableProp paint "text-halo-blur"
        textTranslate <- getTransitionableProp paint "text-translate"
        textTranslateAnchor <- getProp paint "text-translate-anchor"
        pure Symbol {..}
      "raster" -> do
        source <- decodeSourceRef m
        opacity <- getTransitionableProp paint "raster-opacity"
        hueRotate <- getTransitionableProp paint "raster-hue-rotate"
        brightnessMin <- getTransitionableProp paint "raster-brightness-min"
        brightnessMax <- getTransitionableProp paint "raster-brightness-max"
        saturation <- getTransitionableProp paint "raster-saturation"
        contrast <- getTransitionableProp paint "raster-contrast"
        fadeDuration <- getProp paint "raster-fade-duration"
        pure RasterLayer {..}
      "circle" -> do
        source <- decodeSourceRef m
        radius <- getTransitionableProp paint "circle-radius"
        color <- getTransitionableProp paint "circle-color"
        blur <- getTransitionableProp paint "circle-blur"
        opacity <- getTransitionableProp paint "circle-opacity"
        translate <- getTransitionableProp paint "circle-translate"
        translateAnchor <- getProp paint "circle-translate-anchor"
        pitchScale <- getProp paint "circle-pitch-scale"
        pitchAlignment <- getProp paint "circle-pitch-alignment"
        strokeWidth <- getTransitionableProp paint "circle-stroke-width"
        strokeColor <- getTransitionableProp paint "circle-stroke-color"
        strokeOpacity <- getTransitionableProp paint "circle-stroke-opacity"
        pure Circle {..}
      "fill-extrusion" -> do
        source <- decodeSourceRef m
        opacity <- getTransitionableProp paint "fill-extrusion-opacity"
        color <- getTransitionableProp paint "fill-extrusion-color"
        translate <- getTransitionableProp paint "fill-extrusion-translate"
        translateAnchor <- getProp paint "fill-extrusion-translate-anchor"
        pattern <- getTransitionableProp paint  "fill-extrusion-pattern"
        height <- getTransitionableProp paint  "fill-extrusion-height"
        base <- getTransitionableProp paint  "fill-extrusion-base"
        pure FillExtrusion {..}
      "heatmap" -> do
        source <- decodeSourceRef m
        radius <- getTransitionableProp paint "heatmap-radius"
        weight <- getTransitionableProp paint "heatmap-weight"
        intensity <- getTransitionableProp paint "heatmap-intensity"
        hmColor <- getProp paint "heatmap-color"
        opacity <- getTransitionableProp paint "heatmap-opacity"
        pure Heatmap {..}
      "hillshade" -> do
        source <- decodeSourceRef m
        illuminationDirection <- getProp paint "hillshade-illumination-direction"
        illuminationAnchor <- getProp paint "hillshade-illumination-anchor"
        exageration <- getTransitionableProp paint "hillshade-exaggeration"
        shadowColor <- getTransitionableProp paint "hillshade-shadow-color"
        highlightColor <- getTransitionableProp paint "hillshade-highlight-color"
        accentColor <- getTransitionableProp paint "hillshade-accent-color"
        pure Hillshade {..}
      _ -> VendorLayer <$> parseJSON ob

    where
    getProp :: FromJSON a => Object -> Text -> Parser (Maybe a)
    getProp = (.:?)

    getTransitionableProp :: FromJSON a => Object -> Text -> Parser (Maybe (Transitionable a))
    getTransitionableProp o p = do
      mv <- o .:? p
      case mv of
        Just v  -> Just <$> (T <$> pure v <*> o .:? (p<>"-transition"))
        Nothing -> pure Nothing

  parseJSON other = VendorLayer <$> parseJSON other

decodeSourceRef
  :: Object -> Parser SourceRef
decodeSourceRef o = do
  source <- o .: "source"
  sourceLayer <- o .:? "source-layer"
  case sourceLayer of
    Just sl -> pure (VectorSourceRef source sl)
    Nothing -> pure (SourceRef source)

sourceRefPairs :: SourceRef -> [Pair]
sourceRefPairs (VectorSourceRef s l) = ["source".=s, "source-layer".=l]
sourceRefPairs (SourceRef s) = ["source".=s]

instance {-# OVERLAPS #-} FromJSON v => FromJSON [Layer v] where
  parseJSON = mapM parseJSON <=< derefLayers <=< parseJSON

derefLayers :: [Value] -> Parser [Value]
derefLayers ls = do
  byId <- HM.fromList . catMaybes <$> mapM withId ls
  mapM (deref byId) ls
  where
    withId (Object o) = Just <$> ((,) <$> o .: "id" <*> pure o)
    withId _          = pure Nothing
    deref :: HM.HashMap Text Object -> Value -> Parser Value
    deref byId v@(Object layer) = do
      mRef <- layer .:? "ref"
      case mRef of
        Just ref -> do
          parent <- maybe (failT ("invalid ref:" <> ref))
                    pure
                    (ref `HM.lookup` byId)
          let pProps = HM.fromList (catMaybes (map getProp refProps))
              getProp k = (,) <$> pure k <*> (k `HM.lookup` parent)
          pure $ Object (HM.filterWithKey (const . (/="ref")) layer <> pProps)
        Nothing -> pure v
    deref _ v = pure v

    refProps = [ "type", "source", "source-layer", "minzoom", "maxzoom"
               , "filter", "layout"];



type Pixels = Number
type Ems = Number
type Factor = Number
type Degrees = Number


data Anchor = Viewport | Map
  deriving (Eq, Show, Enum, Bounded)

instance ToJSON Anchor where
  toJSON Viewport = "viewport"
  toJSON Map      = "map"

instance FromJSON Anchor where
  parseJSON = withText "Anchor" $ \case
    "viewport" -> pure Viewport
    "map"      -> pure Map
    other      -> failT ("Unexpected anchor: " <> other)

newtype DashArray = DashArray [Number]
  deriving (Eq, Show, ToJSON, FromJSON)

instance IsList DashArray where
  type Item DashArray = Number
  toList (DashArray x) = x
  fromList = DashArray . fromList
  fromListN n = DashArray . fromListN n

newtype FontList = FontList [Text]
  deriving (Eq, Show, ToJSON, FromJSON)

instance IsList FontList where
  type Item FontList = Text
  toList (FontList x) = x
  fromList = FontList . fromList
  fromListN n = FontList . fromListN n

data SourceRef
  = SourceRef Text
  | VectorSourceRef Text Text
  deriving (Eq, Show, Generic)

data Sprite = Sprite
  { width      :: Int
  , height     :: Int
  , x          :: Int
  , y          :: Int
  , pixelRatio :: Double
  } deriving (Eq, Show, Generic)

type SpriteMap = HM.HashMap SpriteId Sprite


newtype SpriteId = SpriteId Text
  deriving (Eq, Show, ToJSON, FromJSON, IsString)

data Transitionable o = T
  { transitionable :: o
  , transition :: Maybe Transition
  }
  deriving (Eq, Show)

-- | Convenience constructor por a transitionable property
tp :: Expr o -> Transitionable (Property o)
tp = flip T Nothing . P

type Milliseconds = Number

data Transition = Transition
  { duration :: Maybe Milliseconds
  , delay    :: Maybe Milliseconds
  }
  deriving (Eq, Show)

instance ToJSON Transition where
  toJSON (Transition{delay,duration}) = object $ catMaybes $
    [ prop "duration" duration
    , prop "delay" delay
    ]

instance FromJSON Transition where
  parseJSON = withObject "transition" $ \o ->
    Transition <$> o .:? "duration" <*> o .:? "delay"

data Property o
  = P (Expr o)
  | IdentityFun
    { base     :: Maybe Number
    , default_ :: Maybe (Expr o)
    , propOrStops  :: Either Text (Stops o)
    , colorSpace :: Maybe ColorSpace
    }
  | ExponentialFun
    { stops    :: Stops o
    , base     :: Maybe Number
    , default_ :: Maybe (Expr o)
    , colorSpace :: Maybe ColorSpace
    }
  | IntervalFun
    { stops    :: Stops o
    , base     :: Maybe Number
    , default_ :: Maybe (Expr o)
    , colorSpace :: Maybe ColorSpace
    }
  | CategoricalFun
    { stops    :: Stops o
    , base     :: Maybe Number
    , default_ :: Maybe (Expr o)
    , colorSpace :: Maybe ColorSpace
    }
  deriving (Eq, Show, Generic)

instance (FromJSON o, FromJSON (Expr o)) => FromJSON (Property o) where
  parseJSON v = withObject "function" parseFun v
            <|> P <$> parseJSON v
    where
    parseFun o = do
      type_ <- o .:? "type" .!= ("identity" :: Text)
      base <- o .:? "base"
      default_ <- o .:? "default"
      colorSpace <- o .:? "colorSpace"
      case type_ of
        "identity" -> do
          propOrStops <- Right <$> decodeStops o
                     <|> Left <$> o.:"property"
          pure (IdentityFun {base,default_,colorSpace,propOrStops})
        "exponential" -> do
          stops <- decodeStops o
          pure (ExponentialFun {stops,base,default_,colorSpace})
        "categorical" -> do
          stops <- decodeStops o
          pure (CategoricalFun {stops,base,default_,colorSpace})
        "interval" -> do
          stops <- decodeStops o
          pure (IntervalFun {stops,base,default_,colorSpace})
        other -> failT ("Unknown function type: " <> other)
    decodeStops o = do
      property <- o .:? "property"
      case property of
        Just p -> ZoomPropStops p <$> o .: "stops"
              <|> PropStops     p <$> o .: "stops"
        Nothing -> ZoomStops      <$> o .: "stops"

instance IsValue o =>  ToJSON (Property o) where
  toJSON = \case
    P (Lit l) -> toJSON l -- Top level literals dont' need to be wrapped in "literal"
    P e -> toJSON e
    ExponentialFun {stops,base,default_,colorSpace} -> object $ catMaybes
      [ Just ("type","exponential")
      , Just ("stops" .= stops)
      , prop "base" base
      , prop "default" default_
      , prop "colorSpace" colorSpace
      , prop "property" (stopsProperty stops)
      ]
    CategoricalFun {stops,base,default_,colorSpace} -> object $ catMaybes
      [ Just ("type","categorical")
      , Just ("stops" .= stops)
      , prop "base" base
      , prop "default" default_
      , prop "colorSpace" colorSpace
      , prop "property" (stopsProperty stops)
      ]
    IntervalFun {stops,base,default_,colorSpace} -> object $ catMaybes
      [ Just ("type","interval")
      , Just ("stops" .= stops)
      , prop "base" base
      , prop "default" default_
      , prop "colorSpace" colorSpace
      , prop "property" (stopsProperty stops)
      ]
    IdentityFun {base,default_,colorSpace,propOrStops=Left p} -> object $ catMaybes
      [ -- Just ("type","identity") default type is "identity"
        prop "base" base
      , prop "default" default_
      , prop "colorSpace" colorSpace
      , Just ("property".=p)
      ]
    IdentityFun {base,default_,colorSpace,propOrStops=Right stops} -> object $ catMaybes
      [ -- Just ("type","identity") default type is "identity"
        prop "base" base
      , prop "default" default_
      , prop "colorSpace" colorSpace
      , prop "property" (stopsProperty stops)
      , Just ("stops" .= stops)
      ]

data ZoomStop o = ZoomStop Zoom o
  deriving (Eq, Show)

instance ToJSON o => ToJSON (ZoomStop o) where
  toJSON (ZoomStop z e) = toJSON [toJSON z, toJSON e]

instance FromJSON o => FromJSON (ZoomStop o) where
  parseJSON = withArray "ZoomStop" $ \a ->
    case V.toList a of
      [z, e] -> ZoomStop <$> parseJSON z <*> parseJSON e
      _      -> failT "Expected 2-item array for zoom function stop"


data PropStop o where
  PropStop :: IsValue b => b -> o -> PropStop o

instance Eq o => Eq (PropStop o) where
  PropStop (a::a) b == PropStop (a'::a') b'
    | Just Refl <- eqT :: Maybe (a :~: a') = a==a' && b==b'
    | otherwise = False

deriving instance Show o => Show (PropStop o)

instance IsValue o => ToJSON (PropStop o) where
  toJSON (PropStop z e) = toJSON [toJSON z, toJSON e]

instance FromJSON o => FromJSON (PropStop o) where
  parseJSON = withArray "PropStop" $ \a ->
    case V.toList a of
      [z, e] -> PropStop <$> parseJSON @Number z <*> parseJSON e
            <|> PropStop <$> parseJSON @Text   z <*> parseJSON e
            <|> PropStop <$> parseJSON @Value  z <*> parseJSON e
      _      -> failT "Expected 2-item array for zoom function stop"


data ZoomPropStop o where
  ZoomPropStop :: IsValue b => Zoom -> b -> o -> ZoomPropStop o

instance Eq o => Eq (ZoomPropStop o) where
  ZoomPropStop a (b::b) c == ZoomPropStop a' (b'::b') c'
    | Just Refl <- eqT :: Maybe (b :~: b') = a==a' && b==b' && c==c'
    | otherwise = False

deriving instance Show o => Show (ZoomPropStop o)

instance IsValue o => ToJSON (ZoomPropStop o) where
  toJSON (ZoomPropStop z v e) = toJSON [object [ "zoom" .=  z, "value" .= v], toJSON e]

instance FromJSON o => FromJSON (ZoomPropStop o) where
  parseJSON = withArray "ZoomPropStop" $ \a ->
    case V.toList a of
      [z, e] -> do
        z' <- parseJSON z
        zoom <- z' .: "zoom"
        ZoomPropStop zoom <$> ((z' .: "value") :: Parser Number) <*> parseJSON e
          <|> ZoomPropStop zoom <$> ((z' .: "value") :: Parser Text) <*> parseJSON e
          <|> ZoomPropStop zoom <$> ((z' .: "value") :: Parser Value) <*> parseJSON e
      _      -> failT "Expected 2-item array for property function stop"

data Stops o
  =  ZoomStops          [ZoomStop o]
  |  PropStops     Text [PropStop o]
  |  ZoomPropStops Text [ZoomPropStop o]
  deriving (Eq, Show, Generic)

stopsProperty :: Stops o -> Maybe Text
stopsProperty (ZoomStops     _  ) = Nothing
stopsProperty (PropStops     p _) = Just p
stopsProperty (ZoomPropStops p _) = Just p

instance IsValue o => ToJSON (Stops o) where
  toJSON (ZoomStops a) = toJSON a
  toJSON (PropStops _ a) = toJSON a
  toJSON (ZoomPropStops _ a) = toJSON a

data ColorSpace = RGBSpace | LabSpace | HclSpace
    deriving (Eq, Show, Enum, Bounded)

instance ToJSON ColorSpace where
  toJSON RGBSpace = "rgb"
  toJSON LabSpace = "lab"
  toJSON HclSpace = "hcl"

instance FromJSON ColorSpace where
  parseJSON = withText "colorSpace" $ \case
    "rgb" -> pure RGBSpace
    "lab" -> pure LabSpace
    "hcl" -> pure HclSpace
    o -> failT ("Invalid color space: " <> o)

data Visibility = NotVisible | Visible
  deriving (Eq, Show, Enum, Bounded)

instance ToJSON Visibility where
  toJSON NotVisible = "none"
  toJSON Visible = "visible"

instance FromJSON Visibility where
  parseJSON = withText "visibility" $ \case
    "none" -> pure NotVisible
    "visible" -> pure Visible
    other -> failT ("Invalid visibility: " <> other)

data LineCap = ButtCap | RoundCap | SquareCap
  deriving (Eq, Show, Enum, Bounded)

instance ToJSON LineCap where
  toJSON ButtCap = "butt"
  toJSON RoundCap = "round"
  toJSON SquareCap = "square"

instance FromJSON LineCap where
  parseJSON = withText "line-cap" $ \case
    "butt" -> pure ButtCap
    "round" -> pure RoundCap
    "square" -> pure SquareCap
    other -> failT ("Invalid line-cap: " <> other)

data LineJoin = BevelJoin | RoundJoin | MiterJoin
  deriving (Eq, Show, Enum, Bounded)

instance ToJSON LineJoin where
  toJSON BevelJoin = "bevel"
  toJSON RoundJoin = "round"
  toJSON MiterJoin = "miter"

instance FromJSON LineJoin where
  parseJSON = withText "line-join" $ \case
    "bevel" -> pure BevelJoin
    "round" -> pure RoundJoin
    "miter" -> pure MiterJoin
    other -> failT ("Invalid line-join: " <> other)

data Padding = Padding
  { paddingTop :: Pixels
  , paddingRight :: Pixels
  , paddingBottom :: Pixels
  , paddingLeft :: Pixels
  } deriving (Eq, Show, Generic)

instance ToJSON Padding where
  toJSON (Padding a b c d) = toJSON [a,b,c,d]

instance FromJSON Padding where
  parseJSON = parseJSON >=> \case
    [a,b,c,d] -> pure (Padding a b c d)
    _     -> failT "expected a 4-element array"

data Translate
data Offset
data XY t = XY Pixels Pixels
  deriving (Eq, Show)

instance ToJSON (XY t) where
  toJSON (XY x y) = toJSON [x,y]

instance FromJSON (XY t) where
  parseJSON = withArray "xy" $ \a ->
    case V.toList a of
      [x,y] -> XY <$> parseJSON x <*> parseJSON y
      _     -> failT "expected a 2-element array"


data SymbolPlacement = PointPlacement | LinePlacement
  deriving (Eq, Show, Enum, Bounded)

instance ToJSON SymbolPlacement where
  toJSON PointPlacement = "point"
  toJSON LinePlacement = "line"

instance FromJSON SymbolPlacement where
  parseJSON = withText "symbol-placement" $ \case
    "line" -> pure LinePlacement
    "point" -> pure PointPlacement
    other -> failT ("Invalid symbol-placement: " <> other)

data Alignment
  = MapAlignment
  | ViewportAlignment
  | AutoAlignment
  deriving (Eq, Show, Enum, Bounded)

instance ToJSON Alignment where
  toJSON MapAlignment = "map"
  toJSON ViewportAlignment = "viewport"
  toJSON AutoAlignment = "auto"

instance FromJSON Alignment where
  parseJSON = withText "alignment" $ \case
    "map" -> pure MapAlignment
    "viewport" -> pure ViewportAlignment
    "auto" -> pure AutoAlignment
    other -> failT ("Invalid alignment: " <> other)

data TextFit
  = FitNone
  | FitWidth
  | FitHeight
  | FitBoth
  deriving (Eq, Show, Enum, Bounded)

instance ToJSON TextFit where
  toJSON FitNone = "none"
  toJSON FitWidth = "width"
  toJSON FitHeight = "height"
  toJSON FitBoth = "both"

instance FromJSON TextFit where
  parseJSON = withText "text-fit" $ \case
    "none" -> pure FitNone
    "width" -> pure FitWidth
    "height" -> pure FitHeight
    "both" -> pure FitBoth
    other -> failT ("Invalid text-fit: " <> other)

data Justify
  = JustifyLeft
  | JustifyCenter
  | JustifyRight
  deriving (Eq, Show, Enum, Bounded)

instance ToJSON Justify where
  toJSON JustifyLeft = "left"
  toJSON JustifyCenter = "center"
  toJSON JustifyRight = "right"

instance FromJSON Justify where
  parseJSON = withText "justify" $ \case
    "left" -> pure JustifyLeft
    "center" -> pure JustifyCenter
    "right" -> pure JustifyRight
    other -> failT ("Invalid justify: " <> other)

data BoxAnchor
  = AnchorCenter
  | AnchorLeft
  | AnchorRight
  | AnchorTop
  | AnchorBottom
  | AnchorTopLeft
  | AnchorTopRight
  | AnchorBottomLeft
  | AnchorBottomRight
  deriving (Eq, Show, Enum, Bounded)

instance ToJSON BoxAnchor where
  toJSON AnchorCenter = "center"
  toJSON AnchorLeft = "left"
  toJSON AnchorRight = "right"
  toJSON AnchorTop = "top"
  toJSON AnchorBottom = "bottom"
  toJSON AnchorTopLeft = "top-left"
  toJSON AnchorTopRight = "top-right"
  toJSON AnchorBottomLeft = "bottom-left"
  toJSON AnchorBottomRight = "bottom-right"

instance FromJSON BoxAnchor where
  parseJSON = withText "box anchor" $ \case
    "center" -> pure AnchorCenter
    "left" -> pure AnchorLeft
    "right" -> pure AnchorRight
    "top" -> pure AnchorTop
    "bottom" -> pure AnchorBottom
    "top-left" -> pure AnchorTopLeft
    "top-right" -> pure AnchorTopRight
    "bottom-left" -> pure AnchorBottomLeft
    "bottom-right" -> pure AnchorBottomRight
    other -> failT ("Invalid anchor: " <> other)

data TextTransform
  = NoneTransform
  | LowercaseTransform
  | UppercaseTransform
  deriving (Eq, Show, Enum, Bounded)

instance ToJSON TextTransform where
  toJSON NoneTransform = "none"
  toJSON LowercaseTransform = "lowercase"
  toJSON UppercaseTransform = "uppercase"

instance FromJSON TextTransform where
  parseJSON = withText "text-transform" $ \case
    "lowercase" -> pure LowercaseTransform
    "uppercase" -> pure UppercaseTransform
    "none" -> pure NoneTransform
    other -> failT ("Invalid text-transform: " <> other)



instance FromJSON (Expr Visibility) where parseJSON = parseExpr
instance FromJSON (Expr SpriteId) where parseJSON = parseExpr
instance FromJSON (Expr LineCap) where parseJSON = parseExpr
instance FromJSON (Expr LineJoin) where parseJSON = parseExpr
instance FromJSON (Expr Anchor) where parseJSON = parseExpr
instance FromJSON (Expr Padding) where parseJSON = parseArray
instance Typeable a => FromJSON (Expr (XY a)) where parseJSON = parseArray
instance FromJSON (Expr SymbolPlacement) where parseJSON = parseExpr
instance FromJSON (Expr Alignment) where parseJSON = parseExpr
instance FromJSON (Expr TextFit) where parseJSON = parseExpr
instance FromJSON (Expr BoxAnchor) where parseJSON = parseExpr
instance FromJSON (Expr Justify) where parseJSON = parseExpr
instance FromJSON (Expr TextTransform) where parseJSON = parseExpr
instance FromJSON (Expr FontList) where parseJSON = parseArray
instance FromJSON (Expr DashArray) where parseJSON = parseArray

instance IsValue Visibility
instance IsValue SpriteId
instance IsValue LineCap
instance IsValue LineJoin
instance IsValue Anchor
instance Typeable a => IsValue (XY a)
instance IsValue SymbolPlacement
instance IsValue Padding
instance IsValue Alignment
instance IsValue TextFit
instance IsValue BoxAnchor
instance IsValue Justify
instance IsValue TextTransform
instance IsValue FontList where
  fromLiteral o = fromLiteralArr o <|> parseJSON o
  toLiteral = toLiteralArr
instance IsValue DashArray

$(deriveJSON defaultOptions ''Sprite)
