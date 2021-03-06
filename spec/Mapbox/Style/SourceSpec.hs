{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
module Mapbox.Style.SourceSpec where

import Data.Proxy (Proxy(..))
import Mapbox.TestUtil
import Mapbox.Style (Source, TileJSON)
import Mapbox.Style.QuickCheck ()
import Test.Hspec

spec :: Spec
spec =  do
    jsonProp (Proxy :: Proxy (Source ()))
    jsonProp (Proxy :: Proxy TileJSON)
