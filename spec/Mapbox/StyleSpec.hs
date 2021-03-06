{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
module Mapbox.StyleSpec (spec) where

import Mapbox.Style (Style)
import Mapbox.Style.Layer (derefLayers)
import Mapbox.Style.QuickCheck ()
import Mapbox.TestUtil
import Data.Aeson
import Data.Aeson.Types (Parser)
import Test.Hspec
import System.FilePath.Glob (compile, globDir1)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V
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

  describe "can parse legacy real-world style" $ do
    styles <- runIO (globDir1 (compile "*.json") "spec/data/styles/legacy")
    forM_ styles $ \fname -> it fname $ do
      bs <- LBS.readFile fname
      eitherDecode @Style bs `shouldSatisfy` isRight
        

newtype RawStyle = RS { unRaw :: Value }
      deriving (Eq, Show)

instance FromJSON RawStyle where
  parseJSON = withObject "raw style" $ \o -> do
    ls <- derefLayers . map normalizeLayerKeys =<< o .: "layers"
    ss <- map normalizeSource <$> (o .: "sources" :: Parser Object)
    let o' = HM.filterWithKey (const . (`notElem` ignoreRootKeys)) o
        ignoreRootKeys =
          ["id", "created", "modified", "owner", "draft", "visibility"]
    pure $ RS $ Data.Aeson.Object
      $ HM.insert "layers" (toJSON ls)
      $ HM.insert "sources" (Object ss)
      $ o'

normalizeSource :: Value -> Value
normalizeSource (Object o) = Object $ HM.filterWithKey go o
 where
    go "grids" (Array o') = not (V.null o')
    go "data"  (Array o') = not (V.null o')
    go _ _ = True
normalizeSource v = v

-- Removes spurious keys some styles keep in the layer objects
-- and normalizes empty paint/layout dicts
normalizeLayerKeys :: Value -> Value
normalizeLayerKeys (Object o) =
  Object $ HM.mapWithKey normalize $ HM.filterWithKey go o
  where
    go "layout" (Object o') = not (HM.null o')
    go "paint" (Object o') = not (HM.null o')
    go "nextDropPoint" _   = False
    go "xray" _   = False
    go k _ | "paint." `T.isPrefixOf` k = False
    go _ _  = True
    normalize "paint" (Object p) = Object (map goProp p)
    normalize "layout" (Object p) = Object (map goProp p)
    normalize _ v = v
    goProp (Object fun) = Object (HM.filterWithKey goFun fun)
    goProp v = v
    goFun "type" "identity" = False
    goFun _ _ = True

normalizeLayerKeys x = x
