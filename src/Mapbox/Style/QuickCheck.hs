{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
module Mapbox.Style.QuickCheck () where

import Mapbox.Style (
    StrMap, Color(..), Number
  , IsValue, Interpolation(..), ArrayCheck(..), Bindings, ExprType
  , Layer, XY(..), Transitionable(..), Transition(..), Property(..)
  , Visibility, SpriteId(..), DashArray(..), SymbolPlacement
  , SourceRef, Anchor, LineCap, LineJoin, Alignment, TextFit
  , BoxAnchor, ColorSpace, Justify, TextTransform, FontList(..)
  , Stops, ZoomStop(..), PropStop(..), ZoomPropStop(..), URI(..)
  , Source, TileJSON, TileScheme, SemVersion, ImageCoordinates, Style'
  , LonLat(..), LonLatZoom(..), MustacheTemplate(..), Bounds(..)
  , Position(..), Light(..), Padding
  , VectorLayer(..), FieldType(..)
  )
import Mapbox.Style.Expression (Expr(..))

import qualified Data.Aeson as Aeson
import Data.Aeson (Value)
import Test.QuickCheck hiding (Property)
import Test.QuickCheck.Arbitrary.Generic (genericArbitrary)
import Test.QuickCheck.Instances ()
import Protolude hiding (Any, All, get)

maxDepth :: Int
maxDepth = 5

lit  :: Arbitrary a => Gen (Expr a)
lit = Lit <$> arbitrary

id_  :: Gen (Expr a)
id_ = pure Id

at  :: (IsValue a, Arbitrary a) => Gen (Expr a)
at = At <$> arbitrary <*> arbitrary

get  :: (IsValue a, Arbitrary a) => Gen (Expr a)
get = Get <$> arbitrary <*> arbitrary

var  :: (IsValue a, Arbitrary a) => Gen (Expr a)
var = Var <$> arbitrary

let_  :: (Arbitrary a, Arbitrary (Expr a)) => Gen (Expr a)
let_ = oneof
  [ letWith (Proxy @Number)
  , letWith (Proxy @Bool)
  , letWith (Proxy @Text)
  --, letWith (Proxy @Color)
  , letWith (Proxy @Value)
  ]

letWith
  :: forall b a
   . (IsValue b, Arbitrary b, Arbitrary (Expr b), Arbitrary a, Arbitrary (Expr a))
  => Proxy b
  -> Gen (Expr a)
letWith _ = scale (min maxDepth) $ sized $ \n -> if n>0
  then resize (n-1) (Let <$> arbitrary @(Bindings b) <*> arbitrary)
  else lit

case_  :: (Arbitrary a, Arbitrary (Expr a)) => Gen (Expr a)
case_ = scale (min maxDepth) $ sized $ \n -> if n>0
  then resize (n-1) (Case <$> arbitrary <*> arbitrary)
  else lit

match_  :: (Arbitrary a, Arbitrary (Expr a)) => Gen (Expr a)
match_ = oneof
  [ matchWith (Proxy @Number)
  , matchWith (Proxy @Text)
  , matchWith (Proxy @Bool)
  , matchWith (Proxy @Value)
  ]

matchWith
  :: forall b a. (IsValue b, Arbitrary b, Arbitrary (Expr b), Arbitrary a, Arbitrary (Expr a))
  => Proxy b -> Gen (Expr a)
matchWith _ = scale (min maxDepth) $ sized $ \n -> if n>0
  then resize (n-1) (Match <$> arbitrary @(Expr b) <*> arbitrary <*> arbitrary)
  else lit

coalesce_  :: (Arbitrary a, Arbitrary (Expr a)) => Gen (Expr a)
coalesce_ = scale (min maxDepth) $ sized $ \n -> if n>0
  then resize (n-1) (Coalesce <$> arbitrary <*> arbitrary <*> arbitrary)
  else lit

expr_  :: (IsValue a, Arbitrary a, Arbitrary (Expr a)) => Gen (Expr a)
expr_ = scale (min maxDepth) $ sized $ \n -> if n==0 then lit else resize (n-1) $ oneof
  [ lit
  , case_
  , coalesce_
  , match_
  , step_
  , at
  , get
  , id_
  , let_
  , var
  ]

interpolableExpr_  :: (IsValue a, Arbitrary a, Arbitrary (Expr a)) => Gen (Expr a)
interpolableExpr_ = oneof
  [ expr_
  , sized $ \n -> if n==0 then lit else resize (n-1) interpolate_
  ]

interpolate_
  :: (Arbitrary a, Arbitrary (Expr a))
  => Gen (Expr a)
interpolate_ = scale (min maxDepth) $ sized $ \n -> if n>0
  then resize (n-1) (Interpolate <$> arbitrary <*> arbitrary <*> arbitrary)
  else lit

step_
  :: (Arbitrary a, Arbitrary (Expr a))
  => Gen (Expr a)
step_ = scale (min maxDepth) $ sized $ \n -> if n>0
  then resize (n-1) (Step <$> arbitrary <*> arbitrary <*> arbitrary)
  else lit


instance Arbitrary Color where
  arbitrary = elements (map (Color . toS) (words "red blue indigo yellow orange black white"))

boolean :: Gen (Expr Bool)
boolean = scaledOneOf
  [ Boolean <$> arbitrary <*> arbitrary
  , ToBoolean <$> arbitrary @(Expr Value)
  , Has <$> arbitrary <*> (arbitrary @(Maybe (Expr (StrMap Value))))
  , NotHas <$> arbitrary <*> (arbitrary @(Maybe (Expr (StrMap Value))))
  , Not <$> arbitrary
  , NotEqual <$> arbitrary @(Expr Value) <*> arbitrary
  , LessThan <$> arbitrary @(Expr Value) <*> arbitrary
  , Equal <$> arbitrary @(Expr Value) <*> arbitrary
  , GreaterThan <$> arbitrary @(Expr Value) <*> arbitrary
  , GreaterThanEq <$> arbitrary @(Expr Value) <*> arbitrary
  , All <$> arbitrary <*> arbitrary
  , Any <$> arbitrary <*> arbitrary
  , None <$> arbitrary <*> arbitrary
  , In <$> arbitrary @(Expr Value) <*> arbitrary
  , NotIn <$> arbitrary  @(Expr Value) <*> arbitrary
  , expr_
  ]

number :: (IsValue a, Arbitrary a, Arbitrary (Expr a)) => Gen (Expr a)
number = scaledOneOf
  [ Number <$> arbitrary <*> arbitrary
  , ToNumber <$> arbitrary <*> arbitrary
  , Length <$> arbitrary @(Expr Value)
  , Minus <$> arbitrary <*> arbitrary
  , Mult <$> arbitrary <*> arbitrary
  , Div <$> arbitrary <*> arbitrary
  , Mod <$> arbitrary <*> arbitrary
  , Pow <$> arbitrary <*> arbitrary
  , Plus <$> arbitrary <*> arbitrary
  , Acos <$> arbitrary
  , Asin <$> arbitrary
  , Atan <$> arbitrary
  , Cos <$> arbitrary
  , pure E
  , Ln <$> arbitrary
  , pure Ln2
  , Log10 <$> arbitrary
  , Log2 <$> arbitrary
  , Max <$> arbitrary <*> arbitrary <*> arbitrary
  , Min <$> arbitrary <*> arbitrary <*> arbitrary
  , pure Pi
  , Sin <$> arbitrary
  , Sqrt <$> arbitrary
  , Tan <$> arbitrary
  , pure Zoom
  , pure HeatmapDensity
  , interpolableExpr_
  ]

string :: Gen (Expr Text)
string = scaledOneOf
  [ String <$> arbitrary <*> arbitrary
  , ToString <$> arbitrary <*> arbitrary
  , TypeOf <$> arbitrary @(Expr Value)
  , Concat <$> arbitrary <*> arbitrary <*> arbitrary
  , Downcase <$> arbitrary
  , Upcase <$> arbitrary
  , pure GeometryType
  , expr_
  ]
  
object :: (IsValue a, Arbitrary a) => Gen (Expr (StrMap a))
object = scaledOneOf
  [ Object <$> arbitrary <*> arbitrary
  , pure Properties
  , expr_
  ]

color_ :: Gen (Expr Color)
color_ = oneof
  [ ToColor <$> arbitrary <*> arbitrary
  , RGB <$> arbitrary <*> arbitrary <*> arbitrary
  , RGBA <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
  , interpolableExpr_
   ]

scaledOneOf :: Arbitrary a => [Gen (Expr a)] -> Gen (Expr a)
scaledOneOf xs = scale (min maxDepth) $ sized $ \n -> if n==0 then lit
  else resize (n-1) (oneof xs)


instance (IsValue a, Arbitrary a) => Arbitrary (Expr [a]) where
  arbitrary = oneof [ array, toRgba, interpolableExpr_ ]
    where
    array = Array <$> arbitrary @(Expr Value) <*> arbitrary
    toRgba = ToRGBA <$> arbitrary
    
instance Arbitrary Aeson.Value where
    arbitrary = scale (min maxDepth) $ sized $ \n ->
        if n > 0
          then
            oneof [ Aeson.Object <$> resize (n-1) arbitrary
                  , Aeson.Array  <$> resize (n-1) arbitrary
                  , Aeson.String  <$> arbitrary
                  , Aeson.Number  <$> arbitrary
                  , pure Aeson.Null
                  ]
          else
            oneof [ Aeson.String  <$> arbitrary
                  , Aeson.Number  <$> arbitrary
                  , pure Aeson.Null
                  ]

instance Arbitrary TileScheme where arbitrary = elements [minBound..maxBound]
instance Arbitrary ExprType where arbitrary = elements [minBound..maxBound]
instance Arbitrary Justify where arbitrary = elements [minBound..maxBound]
instance Arbitrary Anchor where arbitrary = elements [minBound..maxBound]
instance Arbitrary LineCap where arbitrary = elements [minBound..maxBound]
instance Arbitrary LineJoin where arbitrary = elements [minBound..maxBound]
instance Arbitrary Visibility where arbitrary = elements [minBound..maxBound]
instance Arbitrary SymbolPlacement where arbitrary = elements [minBound..maxBound]
instance Arbitrary Alignment where arbitrary = elements [minBound..maxBound]
instance Arbitrary BoxAnchor where arbitrary = elements [minBound..maxBound]
instance Arbitrary ColorSpace where arbitrary = elements [minBound..maxBound]
instance Arbitrary TextFit where arbitrary = elements [minBound..maxBound]
instance Arbitrary TextTransform where arbitrary = elements [minBound..maxBound]

deriving instance Arbitrary URI
deriving instance Arbitrary SpriteId
deriving instance Arbitrary FontList
deriving instance Arbitrary DashArray
deriving instance Arbitrary MustacheTemplate

instance Arbitrary (Expr Aeson.Value) where arbitrary = expr_
instance Arbitrary (Expr SpriteId) where arbitrary = expr_
instance Arbitrary (Expr Justify) where arbitrary = expr_
instance Arbitrary (Expr Padding) where arbitrary = expr_
instance Arbitrary (Expr TextTransform) where arbitrary = expr_
instance Arbitrary (Expr Anchor) where arbitrary = expr_
instance Arbitrary (Expr LineCap) where arbitrary = expr_
instance Arbitrary (Expr BoxAnchor) where arbitrary = expr_
instance Arbitrary (Expr TextFit) where arbitrary = expr_
instance Arbitrary (Expr FontList) where arbitrary = expr_
instance Arbitrary (Expr LineJoin) where arbitrary = expr_
instance Arbitrary (Expr SymbolPlacement) where arbitrary = expr_
instance Arbitrary (Expr Alignment) where arbitrary = expr_
instance Arbitrary (Expr DashArray) where arbitrary = expr_
instance Arbitrary (Expr Bool) where arbitrary = boolean
instance Arbitrary (Expr Color) where arbitrary = color_
instance Arbitrary (Expr Word8) where arbitrary = number
instance Arbitrary (Expr Number) where arbitrary = number
instance Arbitrary (Expr Text) where arbitrary = string
instance (IsValue a, Arbitrary a) => Arbitrary (Expr (StrMap a)) where arbitrary = object

instance Typeable v => Arbitrary (Expr (XY v)) where arbitrary = expr_

instance Arbitrary ArrayCheck where
    arbitrary = ArrayCheck <$> arbitrary
                           <*> arbMay (getPositive <$> arbitrary)

instance Arbitrary Interpolation where
  arbitrary = oneof [ pure Linear
                    , Exponential <$> arbitrary
                    , CubicBezier <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
                    ]

instance Arbitrary x => Arbitrary (Transitionable x) where
  arbitrary = T <$> arbitrary <*> arbitrary

instance Arbitrary Bounds where
  arbitrary = Bounds <$> arbitrary <*> arbitrary

instance Arbitrary LonLatZoom where
  arbitrary = LonLatZoom <$> arbitrary <*> arbitrary

instance Arbitrary LonLat where
  arbitrary = LonLat <$> arbitrary <*> arbitrary

instance Arbitrary Transition where
  arbitrary = Transition <$> arbitrary <*> arbitrary

instance Arbitrary Position where
  arbitrary = Position <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary Light where
  arbitrary = Light <$> arbitrary <*> arbitrary
                    <*> arbitrary <*> arbitrary




instance Arbitrary v => Arbitrary (ZoomStop v) where
  arbitrary = ZoomStop
          <$> (fromIntegral <$> choose @Int (0,32))
          <*> arbitrary

instance Arbitrary v => Arbitrary (PropStop v) where
  arbitrary = PropStop
          <$> arbitrary @Value
          <*> arbitrary

instance Arbitrary v => Arbitrary (ZoomPropStop v) where
  arbitrary = ZoomPropStop
          <$> (fromIntegral <$> choose @Int (0,32))
          <*> arbitrary @Value
          <*> arbitrary




arbMay :: Gen a -> Gen (Maybe a)                           
arbMay a = oneof [pure Nothing, Just <$> a]

instance Arbitrary ImageCoordinates where
  arbitrary = genericArbitrary
  shrink = genericShrink

instance Arbitrary SemVersion where
  arbitrary = genericArbitrary
  shrink = genericShrink

instance Arbitrary TileJSON where
  arbitrary = genericArbitrary
  shrink = genericShrink

instance ( Arbitrary m
         , Arbitrary lm
         , Arbitrary l
         , Arbitrary s
         ) => Arbitrary (Style' m lm l s) where
  arbitrary = scale (min maxDepth) genericArbitrary
  shrink = genericShrink

instance Arbitrary v => Arbitrary (Source v) where
  arbitrary = genericArbitrary
  shrink = genericShrink

instance (Arbitrary m, Arbitrary v) => Arbitrary (Layer m v) where
  arbitrary = genericArbitrary
  shrink = genericShrink

instance (Arbitrary (Expr v), Arbitrary v) => Arbitrary (Stops v) where
  arbitrary = genericArbitrary
  shrink = genericShrink

instance (Arbitrary (Expr v), Arbitrary v) => Arbitrary (Property v) where
  arbitrary = genericArbitrary
  shrink = genericShrink

instance Arbitrary SourceRef where
  arbitrary = genericArbitrary
  shrink = genericShrink

instance Arbitrary Padding where
  arbitrary = genericArbitrary
  shrink = genericShrink

instance Arbitrary FieldType where
  arbitrary = genericArbitrary
  shrink = genericShrink

instance Arbitrary VectorLayer where
  arbitrary = genericArbitrary
  shrink = genericShrink

instance Arbitrary (XY v) where
  arbitrary = XY <$> arbitrary <*> arbitrary
