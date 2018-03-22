{-# LANGUAGE NoImplicitPrelude #-}
module Mapbox.StyleSpec (spec) where

import Mapbox.Style
import Data.Aeson
import Test.Hspec
import System.FilePath.Glob (compile, globDir1)
import qualified Data.ByteString.Lazy as LBS
import Protolude
import Prelude (String)

spec :: Spec
spec = do
  describe "parse styles found in the wild" $ do
    styles <- runIO (globDir1 (compile "*.json") "spec/data/styles")
    forM_ styles $ \s -> it ("can parse "<> s) $ do
      eS <- (eitherDecode <$> LBS.readFile s) :: IO (Either String Style)
      eS `shouldSatisfy` isRight
      --eV <- (eitherDecode <$> LBS.readFile s) :: IO (Either String Value)
      --eV `shouldBe` fmap toJSON eS
        