{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
module Mapbox.Style.LayerSpec where

import Data.Proxy (Proxy(..))
import Mapbox.TestUtil
import Mapbox.Style (Layer)
import Mapbox.Style.QuickCheck ()
import Test.Hspec

spec :: Spec
spec =  do
    laxJsonProp (Proxy :: Proxy (Layer () ()))
