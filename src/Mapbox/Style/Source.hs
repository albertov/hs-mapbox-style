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
  = Vector    (Either URI TileJSON)
  | Raster    (Either URI TileJSON) (Maybe TileSize)
  | RasterDEM (Either URI TileJSON) (Maybe TileSize)
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
  toJSON (Vector (Left v)) = object [("type","vector"), "url".=v]
  toJSON (Vector (Right v)) = injectType "vector" (toJSON v)
  toJSON (Raster (Left v) ts) =
    object (catMaybes [Just ("type","raster"), Just ("url".=v),prop "tileSize" ts])
  toJSON (Raster (Right v) ts) =
    injectPairs (catMaybes [Just ("type","raster"), prop "tileSize" ts]) (toJSON v)
  toJSON (RasterDEM (Left v) ts) =
    object (catMaybes [Just ("type","raster-dem"), Just ("url".=v),prop "tileSize" ts])
  toJSON (RasterDEM (Right v) ts) =
    injectPairs (catMaybes [Just ("type","raster.dem"), prop "tileSize" ts]) (toJSON v)
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


instance FromJSON Source where
  parseJSON v = parseJSON v >>= \o -> do
    ty :: Text <- o .: "type"
    case ty of
      "vector" -> Vector <$>
        (Left <$> o .: "url" <|> Right <$> parseJSON v)
      "raster" ->
        Raster <$> (Left <$> o .: "url" <|> Right <$> parseJSON v)
               <*> o .:? "tileSize"
      "raster-dem" ->
        RasterDEM <$> (Left <$> o .: "url" <|> Right <$> parseJSON v)
                  <*> o .:? "tileSize"
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