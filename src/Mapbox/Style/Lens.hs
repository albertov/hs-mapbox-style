{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
module Mapbox.Style.Lens where

import Mapbox.Style.TH (makeUnderscoreSuffixedFields)
import Mapbox.Style.Expression
import Mapbox.Style.Layer
import Mapbox.Style.Source
import Mapbox.Style.TileJSON
import Mapbox.Style.Types

import Control.Lens

makePrisms ''Expr
makePrisms ''ExprType
makePrisms ''Interpolation
makePrisms ''Anchor
makePrisms ''SourceRef
makePrisms ''Property
makePrisms ''Stops
makePrisms ''ColorSpace
makePrisms ''Visibility
makePrisms ''LineCap
makePrisms ''LineJoin
makePrisms ''SymbolPlacement
makePrisms ''Alignment
makePrisms ''TextFit
makePrisms ''Justify
makePrisms ''BoxAnchor
makePrisms ''TextTransform
makePrisms ''TileScheme
makeUnderscoreSuffixedFields ''Property
makeUnderscoreSuffixedFields ''Source
makeUnderscoreSuffixedFields ''Layer
makeUnderscoreSuffixedFields ''Sprite
makeUnderscoreSuffixedFields ''Transitionable
makeUnderscoreSuffixedFields ''Transition
makeUnderscoreSuffixedFields ''ImageCoordinates
makeUnderscoreSuffixedFields ''TileJSON
makeUnderscoreSuffixedFields ''LonLat
makeUnderscoreSuffixedFields ''LonLatZoom
makeUnderscoreSuffixedFields ''Bounds

tp_ :: Traversal' (Transitionable (Property o)) (Expr o)
tp_ = transitionable_ . _P