{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Mapbox.Style.Types (
  Number
, UnitInterval
, Color (..)
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