{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FunctionalDependencies #-}
module Mapbox.Style (
  Style (..)
, Position (..)
, Light (..)
, style
, HasAnchor(..)
, HasPosition(..)
, HasDistance(..)
, HasAzimuth(..)
, HasElevation(..)

, Expr
, ExprType (..)
, ArrayCheck (..)
, IsValue
, Interpolation (..)
, Bindings
, get
, get'
, at
, has
, has'
, notHas
, notHas'
, not_
, in_
, case_
, step
, match
, notIn
, interpolate
, array'
, array
, rgb
, rgba
, toRgba
, downcase
, upcase
, all_
, concat_
, any_
, none
, length_
, coalesce
, geometryType
, properties
, (.<), (.<=), (.==), (.!=), (.>), (.>=), (.%)

, Source
, ImageCoordinates (..)
, ElementId
, GeoJSONData
, TileSize
, vector
, vectorInline
, raster
, rasterInline
, rasterDEM
, rasterDEMInline
, geoJSON
, image
, video
, canvas

, Anchor (..)
, Layer
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
, Padding
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

, background
, fill
, line
, symbol
, rasterLayer
, circle
, fillExtrusion
, heatmap
, hillshade
, tp

, Number
, UnitInterval
, Bounds (..)
, Color (..)
, URI (..)
, LonLat (..)
, LonLatZoom (..)
, StrMap
, Zoom
, mk1'
, mk1

, module Mapbox.Style.Lens
) where

import Mapbox.Style.Expression
import Mapbox.Style.Source
import Mapbox.Style.Layer
import Mapbox.Style.Types
import Mapbox.Style.Lens
import Mapbox.Style.TH (makeUnderscoreSuffixedFields)

import Data.Aeson ( Value, FromJSON(..), ToJSON(..)
                  , defaultOptions, omitNothingFields)
import Data.Aeson.TH (deriveJSON)

import Protolude hiding (get)
import Prelude (fail)

data Style = Style
  { version    :: Int
  , name       :: Maybe Text
  , metadata   :: Maybe (StrMap Value)
  , center     :: Maybe LonLat
  , zoom       :: Maybe Zoom
  , bearing    :: Maybe Double
  , pitch      :: Maybe Double
  , light      :: Maybe Light
  , sources    :: StrMap Source
  , sprite     :: Maybe URI
  , glyphs     :: Maybe URI
  , transition :: Maybe Transition
  , layers     :: [Layer]
  }
  deriving (Eq, Show)

style :: Style
style = Style
  { version    = 8
  , name       = Nothing
  , metadata   = Nothing
  , center     = Nothing
  , zoom       = Nothing
  , bearing    = Nothing
  , pitch      = Nothing
  , light      = Nothing
  , sources    = mempty
  , sprite     = Nothing
  , glyphs     = Nothing
  , transition = Nothing
  , layers     = []
  }

data Position = Position
  { distance  :: Double
  , azimuth   :: Double
  , elevation :: Double
  }
  deriving (Eq, Show)

instance ToJSON Position where
  toJSON (Position d a e) = toJSON [d, a, e]

instance FromJSON Position where
  parseJSON = parseJSON >=> \case
    [d, a, e] -> pure (Position d a e)
    _         -> fail "invalid position"

data Light = Light
  { anchor    :: Maybe Anchor
  , position  :: Maybe Position
  , color     :: Maybe Color
  , intensity :: Maybe UnitInterval
  }
  deriving (Eq, Show)

$(deriveJSON (defaultOptions { omitNothingFields=True}) ''Light)
$(deriveJSON (defaultOptions { omitNothingFields=True}) ''Style)
makeUnderscoreSuffixedFields ''Light
makeUnderscoreSuffixedFields ''Position