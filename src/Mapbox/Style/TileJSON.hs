{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Mapbox.Style.TileJSON (
  TileJSON (..)
, TileScheme (..)
, MustacheTemplate (..)
, SemVersion (..)
, tileJSON
) where

import Mapbox.Style.Common (prop)
import Mapbox.Style.Types

import Data.Aeson
import Protolude
import qualified Data.Text as T
import Prelude (fail)

data SemVersion = SemVersion Int Int Int
    deriving (Eq, Show, Generic)

instance ToJSON SemVersion where
  toJSON = toJSON . showVersion

instance FromJSON SemVersion where
  parseJSON = maybe (fail "Invalid version") pure . parseSemVersion
          <=< parseJSON

showVersion :: SemVersion -> Text
showVersion (SemVersion major minor patch) =
    show major <> "." <>  show minor <> "." <> show patch

parseSemVersion :: Text -> Maybe SemVersion
parseSemVersion s
  | [major,minor,patch] <- map toS (T.splitOn "." s)
  = SemVersion <$> readMaybe major
               <*> readMaybe minor
               <*> readMaybe patch
parseSemVersion _ = Nothing

-- | Implements TileJSON spec 2.2.0
-- | See: https://github.com/mapbox/tilejson-spec/tree/master/2.2.0

data TileJSON = TileJSON
  { name        :: Maybe Text
  , description :: Maybe Text
  , version     :: Maybe SemVersion
  , attribution :: Maybe Text
  , template    :: Maybe MustacheTemplate
  , legend      :: Maybe Text
  , scheme      :: Maybe TileScheme
  , tiles       :: NonEmpty URI
  , grids       :: [URI]
  , _data       :: [URI]
  , minzoom     :: Maybe Zoom
  , maxzoom     :: Maybe Zoom
  , bounds      :: Maybe Bounds
  , center      :: Maybe LonLatZoom
  } deriving (Eq, Show, Generic)

tileJSON :: URI -> TileJSON
tileJSON uri = TileJSON
  { name        = Nothing
  , description = Nothing
  , version     = Nothing
  , attribution = Nothing
  , template    = Nothing
  , legend      = Nothing
  , scheme      = Nothing
  , tiles       = pure uri
  , grids       = []
  , _data       = []
  , minzoom     = Nothing
  , maxzoom     = Nothing
  , bounds      = Nothing
  , center      = Nothing
  }


instance ToJSON TileJSON where
  toJSON (TileJSON {..}) = object $ catMaybes
    [ prop "name" name
    , prop "description" description
    , prop "version" version
    , prop "attribution" attribution
    , prop "template" template
    , prop "legend" legend
    , prop "scheme" scheme
    , Just ("tiles" .= tiles)
    , prop "grids" (if null grids then Nothing else Just grids)
    , prop "data" (if null _data then Nothing else Just _data)
    , prop "minzoom" minzoom
    , prop "maxzoom" maxzoom
    , prop "bounds" bounds
    , prop "center" center
    ]

instance FromJSON TileJSON where
  parseJSON = withObject "TileJSON" $ \o -> do
    name <- o .:? "name"
    description <- o .:? "description"
    version <- o .:? "version"
    attribution <- o .:? "attribution"
    template <- o .:? "template"
    legend <- o .:? "legend"
    scheme <- o .:? "scheme"
    tiles <- o .: "tiles"
    grids <- o .:? "grids" .!= []
    _data <- o .:? "data" .!= []
    minzoom <- o .:? "minzoom"
    maxzoom <- o .:? "maxzoom"
    bounds <- o .:? "bounds"
    center <- o .:? "center"
    pure TileJSON {..}

data TileScheme = XYZ | TMS
    deriving (Eq, Show, Enum, Bounded)

instance ToJSON TileScheme where
  toJSON XYZ = "xyz"
  toJSON TMS = "tms"

instance FromJSON TileScheme where
  parseJSON = withText "scheme" $ \case
    "xyz" -> pure XYZ
    "tms" -> pure TMS
    _ -> fail "Invalid scheme"

newtype MustacheTemplate = MustacheTemplate Text
  deriving (Eq, Show, IsString, ToJSON, FromJSON)