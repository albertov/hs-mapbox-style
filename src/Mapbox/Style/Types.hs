{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Mapbox.Style.Types (
  Number
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
) where

import Data.Aeson (FromJSON(..), ToJSON(..))
import Data.HashMap.Strict (HashMap)
import Data.Scientific (Scientific)
import Protolude
import Prelude (fail)

type Number = Scientific
type Zoom   = Number
type StrMap = HashMap Text

newtype Color = Color Text
  deriving (Show, Eq, Typeable, IsString, ToJSON, FromJSON)

newtype UnitInterval = UI Number
  deriving (Show, Eq, Typeable, ToJSON)

mk1 :: Number -> Maybe UnitInterval
mk1 v | 0<=v && v<=1 = Just (UI v)
mk1 _ = Nothing

mk1' :: Number -> UnitInterval
mk1' = UI . max 0 . min 1

instance FromJSON UnitInterval where
  parseJSON = maybe (fail "value must be in range [0-1]") pure
          <=< (fmap mk1 . parseJSON)

newtype URI = URI Text
  deriving (Eq, Show, IsString, ToJSON, FromJSON)


data LonLat = LonLat { lon :: !Number, lat :: !Number }
  deriving (Eq, Show)

instance ToJSON LonLat where
  toJSON (LonLat {lon,lat}) = toJSON [lon, lat]

instance FromJSON LonLat where
  parseJSON = parseJSON >=> \case
    [lon,lat] -> pure (LonLat {lon,lat})
    _         -> fail "invalid LonLat"

data Bounds = Bounds { southwest :: !LonLat, northeast :: !LonLat }
  deriving (Eq, Show)

instance ToJSON Bounds where
  toJSON (Bounds {southwest=LonLat {lon=x0,lat=y0}, northeast=LonLat{lon=x1,lat=y1}}) =
    toJSON [x0,y0,x1,y1]

instance FromJSON Bounds where
  parseJSON = parseJSON >=> \case
    [w,s,e,n] -> pure (Bounds {southwest=LonLat {lon=w,lat=s}, northeast=LonLat {lon=e,lat=n}})
    _         -> fail "invalid Bounds"

data LonLatZoom = LonLatZoom { lonLat :: !LonLat, zoom :: !Zoom }
  deriving (Eq, Show)

instance ToJSON LonLatZoom where
    toJSON (LonLatZoom {lonLat=LonLat{lon,lat},zoom}) = toJSON [lon, lat, zoom]

instance FromJSON LonLatZoom where
    parseJSON = parseJSON >=> \case
      [x,y,z] -> pure (LonLatZoom (LonLat x y) z)
      _       -> fail "invalid LonLatZoom"