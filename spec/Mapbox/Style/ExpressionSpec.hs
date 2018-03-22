{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
module Mapbox.Style.ExpressionSpec where

import qualified Data.Aeson as Aeson
import Data.Proxy (Proxy(..))
import Mapbox.TestUtil
import Mapbox.Style.Expression (Expr, Interpolation)
import Mapbox.Style.Types (Number, Color)
import Mapbox.Style.Instances ()
import Data.Text (Text)
import Data.Typeable (Typeable, typeOf)
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck (Arbitrary)

spec :: Spec
spec =  do
    jsonProp (Proxy :: Proxy Interpolation)
    exprProp (Proxy :: Proxy (Expr Aeson.Value))
    exprProp (Proxy :: Proxy (Expr Bool))
    exprProp (Proxy :: Proxy (Expr Number))
    exprProp (Proxy :: Proxy (Expr Aeson.Object))
    exprProp (Proxy :: Proxy (Expr Text))
    exprProp (Proxy :: Proxy (Expr Color))
    exprProp (Proxy :: Proxy (Expr [Aeson.Value]))
    exprProp (Proxy :: Proxy (Expr [Bool]))
    exprProp (Proxy :: Proxy (Expr [Number]))
    exprProp (Proxy :: Proxy (Expr [Aeson.Object]))
    exprProp (Proxy :: Proxy (Expr [Text]))
    exprProp (Proxy :: Proxy (Expr [Color]))


exprProp
  :: forall p a.
   ( Show a
   , Eq a
   , Aeson.ToJSON a
   , Aeson.FromJSON a
   , Arbitrary a
   , Typeable a
   )
  => p a
  -> SpecWith ()
exprProp p = describe (show (typeOf (undefined :: a))) $ do
    laxJsonProp p
    describe "equality" $ do
        prop "is reflexive" $ \(a :: a) -> a == a
        prop "is symmetric" $ \(a :: a, b) -> (a == b) == (b == a)