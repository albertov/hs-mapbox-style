{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
module Mapbox.Style (
  Style (..)
, Position (..)
, Light (..)
, module Mapbox.Style.Expression
, module Mapbox.Style.Source
, module Mapbox.Style.Layer
, module Mapbox.Style.Types
) where

import Mapbox.Style.Expression
import Mapbox.Style.Source
import Mapbox.Style.Layer
import Mapbox.Style.Types

import Data.Aeson
import Data.Aeson.TH

import Protolude
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