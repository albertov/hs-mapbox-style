{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FunctionalDependencies #-}
module Mapbox.Style (
  module Mapbox.Style

, Expr
, ExprType (..)
, ArrayCheck (..)
, IsValue
, Interpolation (..)
, Bindings
, lit
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

, Source (..)
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
, Bounds (..)
, Color (..)
, URI (..)
, LonLat (..)
, LonLatZoom (..)
, StrMap
, Zoom

, TileJSON (..)
, TileScheme (..)
, MustacheTemplate (..)
, SemVersion (..)
, tileJSON

, module Mapbox.Style.Lens
) where

import Mapbox.Style.Expression
import Mapbox.Style.TileJSON
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

type Style = Style' () ()

data Style' l s = Style
  { version    :: Int
  , name       :: Maybe Text
  , metadata   :: Maybe (StrMap Value)
  , center     :: Maybe LonLat
  , zoom       :: Maybe Zoom
  , bearing    :: Maybe Double
  , pitch      :: Maybe Double
  , light      :: Maybe Light
  , sources    :: StrMap (Source s)
  , sprite     :: Maybe URI
  , glyphs     :: Maybe URI
  , transition :: Maybe Transition
  , layers     :: [Layer l]
  }
  deriving (Eq, Show)

style' :: Style' l s
style' = Style
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

style :: Style
style = style'

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
  , intensity :: Maybe Number
  }
  deriving (Eq, Show)

$(deriveJSON (defaultOptions { omitNothingFields=True}) ''Light)
$(deriveJSON (defaultOptions { omitNothingFields=True}) ''Style')
makeUnderscoreSuffixedFields ''Light
makeUnderscoreSuffixedFields ''Style'
makeUnderscoreSuffixedFields ''Position