{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
module Mapbox.StyleSpec (spec) where

import Mapbox.Style (Style)
import Mapbox.Style.Layer (derefLayers)
import Mapbox.Style.QuickCheck ()
import Mapbox.TestUtil
import Data.Aeson
import Test.Hspec
import System.FilePath.Glob (compile, globDir1)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import Protolude

spec :: Spec
spec = do
  laxJsonProp (Proxy @Style)
  describe "parsing a real-world style and serializing it equals original" $ do
    styles <- runIO (globDir1 (compile "*.json") "spec/data/styles")
    forM_ styles $ \fname -> it fname $ do
      bs <- LBS.readFile fname
      let eS = eitherDecode bs
          eV = fmap unRaw (eitherDecode bs)
      case (,) <$> eV <*> eS of
        Right (v,s) -> toJSON (s::Style) `shouldBe` v
        Left err -> expectationFailure err
        

newtype RawStyle = RS { unRaw :: Value }
      deriving (Eq, Show)

instance FromJSON RawStyle where
  parseJSON = withObject "raw style" $ \o -> do
    ls <- derefLayers . map normalizeLayerKeys =<< o .: "layers"
    let o' = HM.filterWithKey (const . (`notElem` ignoreRootKeys)) o
        ignoreRootKeys = ["id", "created", "modified", "owner", "draft"]
    pure $ RS $ Data.Aeson.Object $ HM.insert "layers" (toJSON ls) o'

-- Removes spurious keys some styles keep in the layer objects
-- and normalizes empty paint/layout dicts
normalizeLayerKeys :: Value -> Value
normalizeLayerKeys (Object o) = Object (HM.filterWithKey go o)
  where
    go "layout" (Object o') = not (HM.null o')
    go "paint" (Object o') = not (HM.null o')
    go "nextDropPoint" _   = False
    go "xray" _   = False
    go k _ | "paint." `T.isPrefixOf` k = False
    go _ _  = True
normalizeLayerKeys x = x