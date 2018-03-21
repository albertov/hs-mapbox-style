{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
module Mapbox.Style.Source where

import Mapbox.Style.Common (prop)
import Mapbox.Style.Types (URI, Zoom, Number, LonLat)
import Mapbox.Style.TileJSON (TileJSON)

import Data.Aeson
import Data.Aeson.Types (Pair)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.HashMap.Strict as HM
import Protolude
import Prelude (fail)

data Source
  = Vector    VectorSource
  | Raster    RasterSource
  | RasterDEM RasterSource
  | GeoJSON
    { data_ :: Either URI GeoJSONData
    , maxzoom :: Maybe Zoom
    , buffer :: Maybe Int
    , tolerance :: Maybe Number
    , cluster :: Maybe Bool
    , clusterRadius :: Maybe Number
    , clusterMaxZoom :: Maybe Zoom
    }
  | Image
    { url :: URI
    , coordinates :: ImageCoordinates
    }
  | Video
    { urls :: NonEmpty URI
    , coordinates :: ImageCoordinates
    }
  | Canvas
    { canvas      :: ElementId
    , coordinates :: ImageCoordinates
    , animate     :: Maybe Bool
    }
  deriving (Eq, Show)

data VectorSource
  = VectorUrl URI
  | VectorSource TileJSON
  deriving (Eq, Show)

data RasterSource
  = RasterUrl URI
  | RasterSource TileJSON (Maybe TileSize)
  deriving (Eq, Show)

data ImageCoordinates = ImageCoordinates
  { topLeft :: LonLat
  , topRight :: LonLat
  , bottomRight :: LonLat
  , bottomLeft :: LonLat
  } deriving (Eq, Show)

instance ToJSON ImageCoordinates where
  toJSON ImageCoordinates{..} = toJSON [topLeft, topRight, bottomRight, bottomLeft]

instance FromJSON ImageCoordinates where
  parseJSON = parseJSON >=> \case
    [topLeft, topRight, bottomRight, bottomLeft] -> pure ImageCoordinates{..}
    _ -> fail "invalid image coordinates"

type ElementId = Text
type GeoJSONData = Value
type TileSize = Int

instance ToJSON Source where
  toJSON (Vector (VectorUrl v)) = typed "vector" ["url".=v]
  toJSON (Vector (VectorSource v)) = injectType "vector" (toJSON v)
  toJSON (Raster (RasterUrl v)) = typed "raster" ["url".=v]
  toJSON (Raster (RasterSource v ts)) =
    injectPairs (catMaybes [Just ("type","raster"), prop "tileSize" ts]) (toJSON v)
  toJSON (RasterDEM (RasterUrl v)) = typed "raster-dem" ["url".=v]
  toJSON (RasterDEM (RasterSource v ts)) =
    injectPairs (catMaybes [Just ("type","raster-dem"), prop "tileSize" ts]) (toJSON v)
  toJSON GeoJSON {..} = object $ catMaybes
    [ Just ("type", "geojson")
    , Just ("data", either toJSON toJSON data_)
    , prop "maxzoom" maxzoom
    , prop "buffer" buffer
    , prop "tolerance" tolerance
    , prop "cluster" cluster
    , prop "clusterRadius" clusterRadius
    , prop "clusterMaxZoom" clusterMaxZoom
    ]
  toJSON Image{..} = object [("type", "image"), "url".=url, "coordinates".=coordinates]
  toJSON Video{..} = object [("type", "video"), "urls".=urls, "coordinates".=coordinates]
  toJSON Canvas{..} = object $ catMaybes
    [ Just ("type", "canvas")
    , Just ("canvas".=canvas)
    , Just ("coordinates".=coordinates)
    , prop "animate" animate
    ]

injectType :: Text -> Value -> Value
injectType ty = injectPairs [("type",String ty)]

injectPairs :: [Pair] -> Value -> Value
injectPairs ps (Object o) = Object (foldr (uncurry HM.insert) o ps)
injectPairs _  o          = o


typed :: Text -> [Pair] -> Value
typed ty = object . (("type".=ty):)

instance FromJSON Source where
  parseJSON v = parseJSON v >>= \o -> do
    ty :: Text <- o .: "type"
    case ty of
      "vector" -> Vector <$>
        (VectorUrl <$> o .: "url" <|> VectorSource <$> parseJSON v)
      "raster" -> Raster <$> do
        ts <- o .:? "tileSize"
        (RasterUrl <$> o .: "url" <|> RasterSource <$> parseJSON v <*> pure ts)
      "raster-dem" -> Raster <$> do
        ts <- o .:? "tileSize"
        (RasterUrl <$> o .: "url" <|> RasterSource <$> parseJSON v <*> pure ts)
      "geojson" -> do
        data_ <- Left <$> o .: "data" <|> Right <$> o .: "data"
        maxzoom <- o .:? "maxzoom"
        buffer <- o .:? "buffer"
        tolerance <- o .:? "tolerance"
        cluster <- o .:? "cluster"
        clusterRadius <- o .:? "clusterRadius"
        clusterMaxZoom <- o .:? "clusterMaxZoom"
        pure GeoJSON{..}
      "image" -> Image <$> o .: "url" <*> o.: "coordinates"
      "video" -> Video <$> o .: "urls" <*> o.: "coordinates"
      "canvas" -> Canvas <$> o.:"canvas" <*> o.:"coordinates" <*> o.:?"animate"
      _ -> fail (toS ("unknown source type:" <> ty))