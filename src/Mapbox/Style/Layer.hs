{-# LANGUAGE NoImplicitPrelude #-}
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
module Mapbox.Style.Layer (
  Anchor (..)
, Layer (..)
) where

import Mapbox.Style.Common (failT, prop)
import Mapbox.Style.Expression (Expr, IsValue, parseExpr)
import Mapbox.Style.Types
import Data.Aeson
import Data.Aeson.Types (Parser)
import qualified Data.Vector as V
import Protolude hiding (filter,join)



data Layer
  = Background
    { id          :: Text
    , visibility  :: Maybe (Property Visibility)
    , metadata    :: Maybe (StrMap Value)
    , minzoom     :: Maybe Zoom
    , maxzoom     :: Maybe Zoom
    , filter      :: Maybe (Expr Bool)

    , color       :: Maybe (Transitionable (Property Color))
    , pattern     :: Maybe (Transitionable (Property SpriteRef))
    , opacity     :: Maybe (Transitionable (Property UnitInterval))
    }
  | Fill
    { id          :: Text
    , visibility  :: Maybe (Property Visibility)
    , metadata    :: Maybe (StrMap Value)
    , minzoom     :: Maybe Zoom
    , maxzoom     :: Maybe Zoom
    , filter      :: Maybe (Expr Bool)
    , source      :: SourceRef

    , antialias   :: Maybe (Property Bool)
    , opacity     :: Maybe (Transitionable (Property UnitInterval))
    , color       :: Maybe (Transitionable (Property Color))
    }
  | Line
    { id          :: Text
    , visibility  :: Maybe (Property Visibility)
    , metadata    :: Maybe (StrMap Value)
    , minzoom     :: Maybe Zoom
    , maxzoom     :: Maybe Zoom
    , filter      :: Maybe (Expr Bool)
    , source      :: SourceRef

    , cap         :: Maybe (Property LineCap)
    , join        :: Maybe (Property LineJoin)
    , miterLimit  :: Maybe (Property Pixels)
    , roundLimit  :: Maybe (Property Pixels)
    , opacity     :: Maybe (Transitionable (Property UnitInterval))
    , color       :: Maybe (Transitionable (Property Color))
    , translate   :: Maybe (Transitionable (Property (XY Translate)))
    , translateAnchor :: Maybe (Property Anchor)
    , width  :: Maybe (Transitionable (Property Pixels))
    , gapWidth  :: Maybe (Transitionable (Property Pixels))
    , lineOffset  :: Maybe (Transitionable (Property Pixels))
    , blur  :: Maybe (Transitionable (Property Number))
    , dashArray  :: Maybe (Transitionable (Property [Number]))
    , pattern     :: Maybe (Transitionable (Property SpriteRef))
    }
  | Symbol
    { id          :: Text
    , visibility  :: Maybe (Property Visibility)
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
    , iconTextFitPadding    :: Maybe (Property (XY Padding))
    , iconImage             :: Maybe (Property SpriteRef)
    , iconRotate            :: Maybe (Property Degrees)
    , iconPadding          :: Maybe (Property Pixels)
    , iconKeepUpright     :: Maybe (Property Bool)
    , iconOffset          :: Maybe (Property (XY Offset))
    , iconAnchor          :: Maybe (Property BoxAnchor)
    , iconPitchAlignment :: Maybe (Property Alignment)
    , textPitchAlignment :: Maybe (Property Alignment)
    , textRotationAlignment :: Maybe (Property Alignment)
    , textField             :: Maybe (Property Text)
    , textFont             :: Maybe (Property [Text])
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
    , iconOpacity     :: Maybe (Transitionable (Property UnitInterval))
    , iconColor       :: Maybe (Transitionable (Property Color))
    , iconHaloColor       :: Maybe (Transitionable (Property Color))
    , iconHaloWidth       :: Maybe (Transitionable (Property Pixels))
    , iconHaloBlur       :: Maybe (Transitionable (Property Number))
    , iconTranslate       :: Maybe (Transitionable (Property (XY Translate)))
    , iconTranslateAnchor         :: Maybe (Property Anchor)
    , textOpacity :: Maybe (Transitionable (Property UnitInterval))
    , textColor       :: Maybe (Transitionable (Property Color))
    , textHaloColor       :: Maybe (Transitionable (Property Color))
    , textHaloWidth       :: Maybe (Transitionable (Property Pixels))
    , textHaloBlur       :: Maybe (Transitionable (Property Number))
    , textTranslate       :: Maybe (Transitionable (Property (XY Translate)))
    , textTranslateAnchor         :: Maybe (Property Anchor)
    }
  | Raster
    { id          :: Text
    , visibility  :: Maybe (Property Visibility)
    , metadata    :: Maybe (StrMap Value)
    , minzoom     :: Maybe Zoom
    , maxzoom     :: Maybe Zoom
    , filter      :: Maybe (Expr Bool)
    , source      :: SourceRef

    , opacity     :: Maybe (Transitionable (Property UnitInterval))
    , hueRotate     :: Maybe (Transitionable (Property Degrees))
    , brightnessMin     :: Maybe (Transitionable (Property UnitInterval))
    , brightnessMax     :: Maybe (Transitionable (Property UnitInterval))
    , saturation     :: Maybe (Transitionable (Property Number)) --TODO use a newtype for [-1,-1]
    , contrast     :: Maybe (Transitionable (Property Number)) --TODO ditto
    , fadeDuration     :: Maybe (Property Milliseconds)
    }
  | Circle
    { id          :: Text
    , visibility  :: Maybe (Property Visibility)
    , metadata    :: Maybe (StrMap Value)
    , minzoom     :: Maybe Zoom
    , maxzoom     :: Maybe Zoom
    , filter      :: Maybe (Expr Bool)
    , source      :: SourceRef

    , radius     :: Maybe (Transitionable (Property Pixels))
    , color     :: Maybe (Transitionable (Property Color))
    , blur     :: Maybe (Transitionable (Property Number))
    , opacity     :: Maybe (Transitionable (Property UnitInterval))
    , translate       :: Maybe (Transitionable (Property (XY Translate)))
    , translateAnchor         :: Maybe (Property Anchor)
    , pitchScale         :: Maybe (Property Anchor)
    , pitchAlignment         :: Maybe (Property Anchor)
    , strokeWidth     :: Maybe (Transitionable (Property Pixels))
    , strokeColor     :: Maybe (Transitionable (Property Color))
    , strokeOpacity     :: Maybe (Transitionable (Property UnitInterval))
    }
  | FillExtrusion
    { id          :: Text
    , visibility  :: Maybe (Property Visibility)
    , metadata    :: Maybe (StrMap Value)
    , minzoom     :: Maybe Zoom
    , maxzoom     :: Maybe Zoom
    , filter      :: Maybe (Expr Bool)
    , source      :: SourceRef

    , opacity     :: Maybe (Transitionable (Property UnitInterval))
    , color     :: Maybe (Transitionable (Property Color))
    , translate       :: Maybe (Transitionable (Property (XY Translate)))
    , translateAnchor         :: Maybe (Property Anchor)
    , pattern     :: Maybe (Transitionable (Property SpriteRef))
    , height     :: Maybe (Transitionable (Property Number))
    , base     :: Maybe (Transitionable (Property Number))
    }
  | Heatmap
    { id          :: Text
    , visibility  :: Maybe (Property Visibility)
    , metadata    :: Maybe (StrMap Value)
    , minzoom     :: Maybe Zoom
    , maxzoom     :: Maybe Zoom
    , filter      :: Maybe (Expr Bool)
    , source      :: SourceRef

    , radius     :: Maybe (Transitionable (Property Pixels))
    , weight     :: Maybe (Transitionable (Property Number))
    , intensity     :: Maybe (Transitionable (Property Number))
    , hmColor   :: Maybe (Property Color)
    , opacity     :: Maybe (Transitionable (Property UnitInterval))
    }
  | Hillshade
    { id          :: Text
    , visibility  :: Maybe (Property Visibility)
    , metadata    :: Maybe (StrMap Value)
    , minzoom     :: Maybe Zoom
    , maxzoom     :: Maybe Zoom
    , filter      :: Maybe (Expr Bool)
    , source      :: SourceRef

    , illuminationDirection :: Maybe (Property Degrees)
    , illuminationAnchor :: Maybe (Property Anchor)
    , exageration     :: Maybe (Transitionable (Property UnitInterval))
    , shadowColor     :: Maybe (Transitionable (Property Color))
    , highlightColor     :: Maybe (Transitionable (Property Color))
    , accentColor     :: Maybe (Transitionable (Property Color))
    }
  deriving (Eq, Show)



instance FromJSON Layer where
  parseJSON = withObject "layer" $ \m -> do
    id <- m .: "id"
    metadata <- m .:? "metadata"
    minzoom <- m .:? "minzoom"
    maxzoom <- m .:? "maxzoom"
    filter <- m .:? "filter"

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
        pure (Fill{..})
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
        textLineHeight <- getProp layout "text-max-height"
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
        pure Raster {..}
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
        translateAnchor <- getProp paint "fill-translate-anchor"
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
        highlightColor <- getTransitionableProp paint "hillshade-hightlight-color"
        accentColor <- getTransitionableProp paint "hillshade-accent-color"
        pure Hillshade {..}
      unknown -> failT ("Unknown layer type: " <> unknown)

    where
    decodeSourceRef
      :: Object -> Parser SourceRef
    decodeSourceRef o = do
      source <- o .: "source"
      sourceLayer <- o .:? "sourceLayer"
      case sourceLayer of
        Just sl -> pure (VectorSourceRef source sl)
        Nothing -> pure (SourceRef source)

    getProp :: FromJSON a => Object -> Text -> Parser (Maybe a)
    getProp = (.:?)

    getTransitionableProp :: FromJSON a => Object -> Text -> Parser (Maybe (Transitionable a))
    getTransitionableProp o p = do
      mv <- o .:? p
      case mv of
        Just v  -> Just <$> (Transitionable <$> pure v <*> o .:? (p<>"-transition"))
        Nothing -> pure Nothing




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

data SourceRef
  = SourceRef Text
  | VectorSourceRef Text Text
  deriving (Eq, Show)

newtype SpriteRef = SpriteRef Text
  deriving (Eq, Show, ToJSON, FromJSON, IsString)

data Transitionable o = Transitionable o (Maybe Transition)
  deriving (Eq, Show)

type Milliseconds = Number

data Transition = Transition
  { duration :: Maybe Milliseconds
  , delay    :: Maybe Milliseconds
  }
  deriving (Eq, Show)

instance ToJSON Transition where
  toJSON (Transition{delay,duration}) = object $ catMaybes $
    [ prop "delay" delay
    , prop "duration" duration
    ]

instance FromJSON Transition where
  parseJSON = withObject "transition" $ \o ->
    Transition <$> o .:? "delay" <*> o .:? "duration"

data Property o
  = Prop (Expr o)
  | IdentityFun
    { base     :: Maybe Number
    , default_ :: Maybe (Expr o)
    , property :: Maybe Text
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
  deriving (Eq, Show)

instance FromJSON (Expr o) => FromJSON (Property o) where
  parseJSON v = Prop <$> parseJSON v
        <|> withObject "function" parseFun v
    where
    parseFun o = do
      type_ <- o .:? "type" .!= ("identity" :: Text)
      base <- o .:? "base"
      default_ <- o .:? "default"
      colorSpace <- o .:? "colorSpace"
      case type_ of
        "identity" -> do
          property <- o .:? "property"
          pure (IdentityFun {base,property,default_,colorSpace})
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
    Prop e -> toJSON e
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
    IdentityFun {property,base,default_,colorSpace} -> object $ catMaybes
      [ Just ("type","identity")
      , prop "base" base
      , prop "default" default_
      , prop "colorSpace" colorSpace
      , prop "property" property
      ]

data ZoomStop o = ZoomStop Zoom (Expr o)
  deriving (Eq, Show)

instance ToJSON (Expr o) => ToJSON (ZoomStop o) where
  toJSON (ZoomStop z e) = toJSON [toJSON z, toJSON e]

instance FromJSON (Expr o) => FromJSON (ZoomStop o) where
  parseJSON = withArray "ZoomStop" $ \a ->
    case V.toList a of
      [z, e] -> ZoomStop <$> parseJSON z <*> parseJSON e
      _      -> failT "Expected 2-item array for zoom function stop"


data PropStop o where
  PropStop :: IsValue b => b -> Expr o -> PropStop o

instance Eq o => Eq (PropStop o) where
  PropStop (a::a) b == PropStop (a'::a') b'
    | Just Refl <- eqT :: Maybe (a :~: a') = a==a' && b==b'
    | otherwise = False

deriving instance Show o => Show (PropStop o)

instance IsValue o => ToJSON (PropStop o) where
  toJSON (PropStop z e) = toJSON [toJSON z, toJSON e]

instance FromJSON (Expr o) => FromJSON (PropStop o) where
  parseJSON = withArray "PropStop" $ \a ->
    case V.toList a of
      [z, e] -> PropStop <$> parseJSON @Number z <*> parseJSON e
            <|> PropStop <$> parseJSON @Text   z <*> parseJSON e
            <|> PropStop <$> parseJSON @Value  z <*> parseJSON e
      _      -> failT "Expected 2-item array for zoom function stop"


data ZoomPropStop o where
  ZoomPropStop :: IsValue b => Zoom -> b -> Expr o -> ZoomPropStop o

instance Eq o => Eq (ZoomPropStop o) where
  ZoomPropStop a (b::b) c == ZoomPropStop a' (b'::b') c'
    | Just Refl <- eqT :: Maybe (b :~: b') = a==a' && b==b' && c==c'
    | otherwise = False

deriving instance Show o => Show (ZoomPropStop o)

instance IsValue o => ToJSON (ZoomPropStop o) where
  toJSON (ZoomPropStop z v e) = toJSON [object [ "zoom" .=  z, "value" .= v], toJSON e]

instance FromJSON (Expr o) => FromJSON (ZoomPropStop o) where
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
  =  ZoomStops            [ZoomStop o]
  |  PropStops     Text [PropStop o]
  |  ZoomPropStops Text [ZoomPropStop o]
  deriving (Eq, Show)

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

data Visibility = None | Visible
  deriving (Eq, Show, Enum, Bounded)

instance ToJSON Visibility where
  toJSON None = "none"
  toJSON Visible = "visible"

instance FromJSON Visibility where
  parseJSON = withText "visibility" $ \case
    "none" -> pure None
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

data Padding
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
instance FromJSON (Expr SpriteRef) where parseJSON = parseExpr
instance FromJSON (Expr LineCap) where parseJSON = parseExpr
instance FromJSON (Expr LineJoin) where parseJSON = parseExpr
instance FromJSON (Expr Anchor) where parseJSON = parseExpr
instance Typeable a => FromJSON (Expr (XY a)) where parseJSON = parseExpr
instance FromJSON (Expr SymbolPlacement) where parseJSON = parseExpr
instance FromJSON (Expr Alignment) where parseJSON = parseExpr
instance FromJSON (Expr TextFit) where parseJSON = parseExpr
instance FromJSON (Expr BoxAnchor) where parseJSON = parseExpr
instance FromJSON (Expr Justify) where parseJSON = parseExpr
instance FromJSON (Expr TextTransform) where parseJSON = parseExpr

instance IsValue Visibility
instance IsValue SpriteRef
instance IsValue LineCap
instance IsValue LineJoin
instance IsValue Anchor
instance Typeable a => IsValue (XY a)
instance IsValue SymbolPlacement
instance IsValue Alignment
instance IsValue TextFit
instance IsValue BoxAnchor
instance IsValue Justify
instance IsValue TextTransform