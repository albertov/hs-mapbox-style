{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
module Mapbox.StyleSpec (spec) where

import Mapbox.Style (Style)
import Mapbox.Style.Layer (derefLayers)
import Mapbox.Style.QuickCheck ()
import Mapbox.TestUtil
import Data.Aeson
import Data.Aeson.Key (toText)
import Data.Aeson.Types (Parser)
import Test.Hspec
import System.FilePath.Glob (compile, globDir1)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Aeson.KeyMap as KeyMap
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
    let o' = KeyMap.filterWithKey (const . (`notElem` ignoreRootKeys)) o
        ignoreRootKeys =
          ["id", "created", "modified", "owner", "draft", "visibility"]
    pure $ RS $ Data.Aeson.Object
      $ KeyMap.insert "layers" (toJSON ls)
      $ KeyMap.insert "sources" (Object ss)
      $ o'

normalizeSource :: Value -> Value
normalizeSource (Object o) = Object $ KeyMap.filterWithKey go o
 where
    go "grids" (Array o') = not (V.null o')
    go "data"  (Array o') = not (V.null o')
    go _ _ = True
normalizeSource v = v

-- Removes spurious keys some styles keep in the layer objects
-- and normalizes empty paint/layout dicts
normalizeLayerKeys :: Value -> Value
normalizeLayerKeys (Object o) =
  Object $ runIdentity $ KeyMap.traverseWithKey normalize $ KeyMap.filterWithKey go o
  where
    go (toText -> "layout") (Object o') = not (KeyMap.null o')
    go (toText -> "paint") (Object o') = not (KeyMap.null o')
    go (toText -> "nextDropPoint") _   = False
    go (toText -> "xray") _   = False
    go k _ | "paint." `T.isPrefixOf` (toText k) = False
    go _ _  = True
    normalize (toText -> "paint") (Object p) = pure $ Object (map goProp p)
    normalize (toText -> "layout") (Object p) = pure $ Object (map goProp p)
    normalize _ v = pure v
    goProp (Object fun) = Object (KeyMap.filterWithKey goFun fun)
    goProp v = v
    goFun (toText -> "type") "identity" = False
    goFun _ _ = True

normalizeLayerKeys x = x
