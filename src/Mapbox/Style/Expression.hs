{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
module Mapbox.Style.Expression ( 
  Expr (..)
, ExprType (..)
, ArrayCheck (..)
, IsValue (..)
, Color (..)
, Interpolation (..)
, StrMap
, Bindings
, UnitInterval
, Number
, mk1'
, mk1
, (.<), (.<=), (.==), (.!=), (.>), (.>=), (.%)
) where

import Data.Aeson (Value, FromJSON(..), ToJSON(..), withText, withArray)
import Data.Aeson.Types (Parser)
import Data.HashMap.Strict (HashMap)
import Data.Scientific
import Data.Typeable
import qualified Data.Aeson as Aeson
import qualified Data.Vector as V
import Data.Word (Word8)
import Protolude hiding (Any, All)
import Prelude (fail)

type Number = Scientific

data Expr a where
  Array :: IsValue b => Expr b -> Maybe ArrayCheck -> Expr [a]
  Boolean :: Expr Value -> [Expr Value] -> Expr Bool
  Lit   :: a -> Expr a
  Number :: Expr Value -> [Expr Value] -> Expr a
  Object :: Expr Value -> [Expr Value] -> Expr (StrMap a)
  String :: Expr Value -> [Expr Value] -> Expr Text
  ToBoolean :: IsValue b => Expr b -> Expr Bool
  ToColor :: Expr Value -> [Expr Value] -> Expr Color
  ToNumber :: Expr Value -> [Expr Value] -> Expr a
  ToString :: Expr Value -> [Expr Value] -> Expr Text
  TypeOf :: IsValue b => Expr b -> Expr Text
  GeometryType :: Expr Text
  Id           :: Expr a
  Properties :: Expr (StrMap a)
  At         :: Int -> Expr [a] -> Expr a
  Get         :: Text -> Maybe (Expr (StrMap a)) -> Expr a
  Has         :: IsValue b => Text -> Maybe (Expr (StrMap b)) -> Expr Bool
  NotHas      :: IsValue b => Text -> Maybe (Expr (StrMap b)) -> Expr Bool
  Length :: IsValue b => Expr b -> Expr a
  Not    :: Expr Bool -> Expr Bool
  NotEqual    :: (Eq b, IsValue b) => Expr b -> Expr b -> Expr Bool
  LessThan :: IsValue b => Expr b -> Expr b -> Expr Bool
  LessThanEq :: IsValue b => Expr b -> Expr b -> Expr Bool
  Equal :: (Eq b, IsValue b) => Expr b -> Expr b -> Expr Bool
  GreaterThan :: IsValue b => Expr b -> Expr b -> Expr Bool
  GreaterThanEq :: IsValue b => Expr b -> Expr b -> Expr Bool
  All :: Expr Bool -> Expr Bool -> [Expr Bool] -> Expr Bool
  Any :: Expr Bool -> Expr Bool -> [Expr Bool] -> Expr Bool
  None :: Expr Bool -> Expr Bool -> [Expr Bool] -> Expr Bool
  In   :: IsValue b => Expr b -> [Expr b] -> Expr Bool
  NotIn :: IsValue b => Expr b -> [Expr b] -> Expr Bool
  Case :: [(Expr Bool, Expr a)] -> Expr a -> Expr a
  Coalesce :: Expr a -> Expr a -> [Expr a] -> Expr a
  Match :: IsValue b => Expr b -> [(Expr b, Expr a)] -> Expr a -> Expr a
  Interpolate :: Interpolation -> Expr Number -> [(Expr Number, Expr a)] -> Expr a
  Step :: Expr Number -> Expr a -> [(Expr Number, Expr a)] -> Expr a
  Let :: IsValue b => Bindings b -> Expr a -> Expr a
  Var :: Text -> Expr a
  Concat :: Expr Text -> Expr Text -> [Expr Text] -> Expr Text
  Downcase :: Expr Text -> Expr Text
  Upcase :: Expr Text -> Expr Text
  RGB    :: Expr Word8 -> Expr Word8 -> Expr Word8 -> Expr Color
  RGBA   :: Expr Word8 -> Expr Word8 -> Expr Word8 -> Expr UnitInterval -> Expr Color
  ToRGBA :: Expr Color -> Expr [a]
  Minus   :: Expr a -> Expr a -> Expr a
  Mult    :: Expr a -> Expr a -> Expr a
  Div     :: Expr a -> Expr a -> Expr a
  Mod     :: Expr a -> Expr a -> Expr a
  Pow     :: Expr a -> Expr a -> Expr a
  Plus    :: Expr a -> Expr a -> Expr a
  Acos    :: Expr a -> Expr a
  Asin    :: Expr a -> Expr a
  Atan    :: Expr a -> Expr a
  Cos    :: Expr a -> Expr a
  E       :: Expr a
  Ln      :: Expr a -> Expr a
  Ln2     :: Expr a
  Log10   :: Expr a -> Expr a
  Log2    :: Expr a -> Expr a
  Max     :: Expr a -> Expr a -> [Expr a] -> Expr a
  Min     :: Expr a -> Expr a -> [Expr a] -> Expr a
  Pi      :: Expr a
  Sin    :: Expr a -> Expr a
  Sqrt    :: Expr a -> Expr a
  Tan    :: Expr a -> Expr a
  Zoom      :: Expr a
  HeatmapDensity :: Expr a

infix 4 .<
(.<) :: IsValue a => Expr a -> Expr a -> Expr Bool
(.<) = LessThan

infix 4 .<=
(.<=) :: IsValue a => Expr a -> Expr a -> Expr Bool
(.<=) = LessThanEq

infix 4 .>
(.>) :: IsValue a => Expr a -> Expr a -> Expr Bool
(.>) = GreaterThan

infix 4 .>=
(.>=) :: IsValue a => Expr a -> Expr a -> Expr Bool
(.>=) = GreaterThanEq

infix 4 .==
(.==) :: IsValue a => Expr a -> Expr a -> Expr Bool
(.==) = Equal

infix 4 .!=
(.!=) :: IsValue a => Expr a -> Expr a -> Expr Bool
(.!=) = NotEqual

infix 7 .%
(.%) :: IsValue a => Expr a -> Expr a -> Expr a
(.%) = Mod


instance (IsValue a, Num a) => Num (Expr a) where
  fromInteger = Lit . fromInteger
  (+) = Plus
  (-) = Minus
  (*) = Mult
  abs e = Case [ (e `LessThan` 0, negate e) ] e
  signum e = Case [ (e `LessThan` 0, -1) ] 1

instance (IsValue a, Fractional a) => Fractional (Expr a) where
  fromRational = Lit . fromRational
  (/) = Div

instance (Fractional a, IsValue a) => Floating (Expr a) where
  pi = Pi
  exp a = E ** a
  log = Ln
  sqrt = Sqrt
  (**) = Pow
  logBase b x = Log10 x / Log10 b
  sin = Sin
  cos = Cos
  tan = Tan
  asin = Asin
  acos = Acos
  atan = Atan
  sinh x = (1 - E ** ((-2) * x)) / ((2 * E) ** (-x))
  cosh x = (1 + E ** ((-2) * x)) / ((2 * E) ** (-x))
  tanh x = (1 - E ** ((-2) * x)) / (1 + E ** ((-2) * x))
  asinh x = log (x + sqrt (x*x+1))
  acosh x = log (x + sqrt (x+1) * sqrt (x-1))
  atanh x = (log (1+x) - log (1-x)) / 2


type Bindings b = NonEmpty (Text, Expr b)

deriving instance Show a => Show (Expr a)
instance Eq x => Eq (Expr x) where
  Array (a :: Expr a) b == Array (a' :: Expr a') b'
    | Just Refl <- (eqT :: Maybe (a :~: a')) = a == a' && b == b'
  Boolean a b == Boolean a' b' = a == a' && b == b'
  Lit a == Lit a' = a == a'
  Number a b == Number a' b' = a == a' && b == b'
  Object a b == Object a' b' = a == a' && b == b'
  String a b == String a' b' = a == a' && b == b'
  ToBoolean (a :: Expr a) == ToBoolean (a' :: Expr a')
    | Just Refl <- (eqT :: Maybe (a :~: a')) = a == a'
  ToColor a b == ToColor a' b' = a == a' && b == b'
  ToNumber a b == ToNumber a' b' = a == a' && b == b'
  ToString a b == ToString a' b' = a == a' && b == b'
  TypeOf (a :: Expr a) == TypeOf (a' :: Expr a')
    | Just Refl <- (eqT :: Maybe (a :~: a')) = a == a'
  GeometryType == GeometryType = True
  Id == Id = True
  Properties == Properties = True
  At a b == At a' b' = a == a' && b == b'
  Get a b == Get a' b' = a == a' && b == b'
  Has a (b :: Maybe (Expr (StrMap b))) == Has a' (b' :: Maybe (Expr (StrMap b')))
    | Just Refl <- (eqT :: Maybe (b :~: b')) = a == a' && b == b'
  NotHas a (b :: Maybe (Expr (StrMap b))) == NotHas a' (b' :: Maybe (Expr (StrMap b')))
    | Just Refl <- (eqT :: Maybe (b :~: b')) = a == a' && b == b'
  Length (a :: Expr a) == Length (a' :: Expr a')
    | Just Refl <- (eqT :: Maybe (a :~: a')) = a == a'
  Not a == Not a' = a == a'
  NotEqual (a :: Expr a) b == NotEqual (a' :: Expr a') b'
    | Just Refl <- (eqT :: Maybe (a :~: a')) = a == a' && b == b'
  In (a :: Expr a) b == In (a' :: Expr a') b'
    | Just Refl <- (eqT :: Maybe (a :~: a')) = a == a' && b == b'
  NotIn (a :: Expr a) b == NotIn (a' :: Expr a') b'
    | Just Refl <- (eqT :: Maybe (a :~: a')) = a == a' && b == b'
  LessThan (a :: Expr a) b == LessThan (a' :: Expr a') b'
    | Just Refl <- (eqT :: Maybe (a :~: a')) = a == a' && b == b'
  LessThanEq (a :: Expr a) b == LessThanEq (a' :: Expr a') b'
    | Just Refl <- (eqT :: Maybe (a :~: a')) = a == a' && b == b'
  Equal (a :: Expr a) b == Equal (a' :: Expr a') b'
    | Just Refl <- (eqT :: Maybe (a :~: a')) = a == a' && b == b'
  GreaterThan (a :: Expr a) b == GreaterThan (a' :: Expr a') b'
    | Just Refl <- (eqT :: Maybe (a :~: a')) = a == a' && b == b'
  GreaterThanEq (a :: Expr a) b == GreaterThanEq (a' :: Expr a') b'
    | Just Refl <- (eqT :: Maybe (a :~: a')) = a == a' && b == b'
  All a b c == All a' b' c' = a == a' && b == b' && c == c'
  Any a b c == Any a' b' c' = a == a' && b == b' && c == c'
  None a b c == None a' b' c' = a == a' && b == b' && c == c'
  Case a b == Case a' b' = a == a' && b == b'
  Coalesce a b c == Coalesce a' b' c' = a == a' && b == b' && c == c'
  Match (a :: Expr a) b c == Match (a' :: Expr a') b' c'
    | Just Refl <- (eqT :: Maybe (a:~:a')) = a == a' && b == b' && c == c'
  Interpolate a b c == Interpolate a' b' c' = a == a' && b == b' && c == c'
  Step a b c == Step a' b' c' = a == a' && b == b' && c == c'
  Let (a :: Bindings a) b  == Let (a' :: Bindings a') b'
    | Just Refl <- (eqT :: Maybe (a:~:a')) = a == a' && b == b'
  Var a == Var a' = a == a'
  Concat a b c == Concat a' b' c' = a == a' && b == b' && c == c'
  Downcase a == Downcase a' = a == a'
  Upcase a == Upcase a' = a == a'
  RGB a b c == RGB a' b' c' = a == a' && b == b' && c == c'
  RGBA a b c d == RGBA a' b' c' d' = a == a' && b == b' && c == c' && d==d'
  ToRGBA a == ToRGBA a' = a == a'
  Minus a b == Minus a' b' = a == a' && b == b'
  Mult  a b == Mult  a' b' = a == a' && b == b'
  Div  a b == Div  a' b' = a == a' && b == b'
  Mod  a b == Mod  a' b' = a == a' && b == b'
  Pow  a b == Pow  a' b' = a == a' && b == b'
  Plus  a b == Plus  a' b' = a == a' && b == b'
  Acos a == Acos a' = a == a'
  Asin a == Asin a' = a == a'
  Atan a == Atan a' = a == a'
  Cos a == Cos a' = a == a'
  E == E = True
  Ln a == Ln a' = a == a'
  Ln2 == Ln2 = True
  Log10 a == Log10 a' = a == a'
  Log2 a == Log2 a' = a == a'
  Max a b c == Max a' b' c' = a == a' && b == b' && c == c'
  Min a b c == Min a' b' c' = a == a' && b == b' && c == c'
  Pi == Pi = True
  Sin a == Sin a' = a == a'
  Sqrt a == Sqrt a' = a == a'
  Tan a == Tan a' = a == a'
  Zoom == Zoom = True
  HeatmapDensity == HeatmapDensity = True
  _ == _ = False



instance IsValue a => ToJSON (Expr a) where
  toJSON (Lit a) = toLiteral a

  toJSON (Array a Nothing) = op "array" a
  toJSON (Array a (Just (ArrayCheck t Nothing))) = op2 "array" t a
  toJSON (Array a (Just (ArrayCheck t (Just l)))) = op3 "array" t l a

  toJSON (Boolean       a as   ) = opArgs  "boolean"       a as
  toJSON (Number        a as   ) = opArgs  "number"        a as
  toJSON (Object        a as   ) = opArgs  "object"        a as
  toJSON (String        a as   ) = opArgs  "string"        a as
  toJSON (ToBoolean     a      ) = op      "to-boolean"    a
  toJSON (ToColor       a as   ) = opArgs  "to-color"      a as
  toJSON (ToNumber      a as   ) = opArgs  "to-number"     a as
  toJSON (ToString      a as   ) = opArgs  "to-string"     a as
  toJSON (TypeOf        a      ) = op      "typeof"        a
  toJSON  GeometryType           = constE  "geometry-type"
  toJSON  Id                     = constE  "id"
  toJSON  Properties             = constE  "properties"
  toJSON (At            a b    ) = op2     "at"            a b
  toJSON (Get    a (Just b)    ) = op2     "get"           a b
  toJSON (Get    a Nothing     ) = op      "get"           a
  toJSON (Has    a (Just b)    ) = op2     "has"           a b
  toJSON (Has    a Nothing     ) = op      "has"           a
  toJSON (NotHas a (Just b)    ) = op2     "!has"          a b
  toJSON (NotHas a Nothing     ) = op      "!has"          a
  toJSON (Length        a      ) = op      "length"        a
  toJSON (Not           a      ) = op      "!"             a
  toJSON (NotEqual      a b    ) = op2     "!="            a b
  toJSON (LessThan      a b    ) = op2     "<"             a b
  toJSON (LessThanEq    a b    ) = op2     "<="            a b
  toJSON (Equal         a b    ) = op2     "=="            a b
  toJSON (GreaterThan   a b    ) = op2     ">"             a b
  toJSON (GreaterThanEq a b    ) = op2     ">="            a b
  toJSON (All           a b c  ) = op2Args "all"           a b c
  toJSON (Any           a b c  ) = op2Args "any"           a b c
  toJSON (In            a b    ) = opArgs  "in"            a b
  toJSON (NotIn         a b    ) = opArgs  "!in"           a b
  toJSON (None          a b c  ) = op2Args "none"          a b c
  toJSON (Coalesce      a b c  ) = op2Args "coalesce"      a b c
  toJSON (Var           a      ) = op      "var"           a
  toJSON (Concat        a b c  ) = op2Args "concat"        a b c
  toJSON (Downcase      a      ) = op      "downcase"      a
  toJSON (Upcase        a      ) = op      "upcase"        a
  toJSON (RGB           a b c  ) = op3     "rgb"           a b c
  toJSON (RGBA          a b c d) = op4     "rgba"          a b c d
  toJSON (ToRGBA        a      ) = op      "to-rgba"       a
  toJSON (Minus         a b    ) = op2     "-"             a b
  toJSON (Mult          a b    ) = op2     "*"             a b
  toJSON (Div           a b    ) = op2     "/"             a b
  toJSON (Mod           a b    ) = op2     "%"             a b
  toJSON (Pow           a b    ) = op2     "^"             a b
  toJSON (Plus          a b    ) = op2     "+"             a b
  toJSON (Acos          a      ) = op      "acos"          a
  toJSON (Asin          a      ) = op      "asin"          a
  toJSON (Atan          a      ) = op      "atan"          a
  toJSON (Cos           a      ) = op      "cos"           a
  toJSON E                       = constE  "e"
  toJSON (Ln            a      ) = op      "ln"            a
  toJSON Ln2                     = constE  "ln2"
  toJSON (Log10         a      ) = op      "log10"         a
  toJSON (Log2          a      ) = op      "log2"          a
  toJSON (Max           a b c  ) = op2Args "max"           a b c
  toJSON (Min           a b c  ) = op2Args "min"           a b c
  toJSON Pi                      = constE  "pi"
  toJSON (Sin           a      ) = op      "sin"           a
  toJSON (Sqrt          a      ) = op      "sqrt"          a
  toJSON (Tan           a      ) = op      "tan"           a
  toJSON Zoom                    = constE  "zoom"
  toJSON HeatmapDensity          = constE  "heatmap-density"

  toJSON (Case cases dflt) = toJSON (label "case": concatMap (\(c,v) -> [toJSON c, toJSON v]) cases <> [toJSON dflt])
  toJSON (Match input cases dflt) =
    toJSON (label "match": toJSON input:concatMap (\(c,v) -> [toJSON c, toJSON v]) cases <> [toJSON dflt])
  toJSON (Interpolate type_ input cases) =
    toJSON (label "interpolate":toJSON type_:toJSON input:concatMap (\(c,v) -> [toJSON c, toJSON v]) cases)
  toJSON (Step input out0 cases) =
    toJSON (label "step":toJSON input:toJSON out0:concatMap (\(c,v) -> [toJSON c, toJSON v]) cases)
  toJSON (Let bs x) =
    toJSON (label "let":concatMap (\(c,v) -> [toJSON c, toJSON v]) bs <> [toJSON  x])


instance FromJSON (Expr Value) where parseJSON = parseValue
instance FromJSON (Expr UnitInterval) where parseJSON = parseNumber
instance FromJSON (Expr Word8) where parseJSON = parseNumber
instance FromJSON (Expr Number) where parseJSON = parseNumber
instance IsValue o => FromJSON (Expr (StrMap o)) where parseJSON = parseObject
instance FromJSON (Expr Text) where parseJSON = parseString
instance FromJSON (Expr Color) where parseJSON = parseColor
instance FromJSON (Expr Bool) where parseJSON = parseBool
instance IsValue a => FromJSON (Expr [a]) where
  parseJSON o = parseWith go o
            <|> parseInterpolate o
            <|> parseExpr o
    where
    go "array" [e] = Array <$> parseJSON @(Expr Value) e <*> pure Nothing
    go "array" [t, e] = Array <$> parseJSON @(Expr Value) e <*> (Just <$> (ArrayCheck <$> parseJSON t <*> pure Nothing))
    go "array" [t, l, e] = Array <$> parseJSON @(Expr Value) e <*> (Just <$> (ArrayCheck <$> parseJSON t <*> (Just <$> parseJSON l)))
    go "to-rgba" [c] = ToRGBA <$> parseJSON c
    go other _ = fail (toS ("Unknown expression type" <> other))


label :: Text -> Value
label = Aeson.String

newtype Color = Color Text
  deriving (Show, Eq, Typeable, IsString, ToJSON, FromJSON, IsValue)

newtype UnitInterval = UI Number
  deriving (Show, Eq, Typeable, ToJSON, IsValue)

mk1 :: Number -> Maybe UnitInterval
mk1 v | 0<=v && v<=1 = Just (UI v)
mk1 _ = Nothing

mk1' :: Number -> UnitInterval
mk1' = UI . max 0 . min 1

instance FromJSON UnitInterval where
  parseJSON = maybe (fail "value must be in range [0-1]") pure
          <=< (fmap mk1 . parseJSON)

data ExprType = StringT | NumberT | BooleanT
  deriving (Show, Eq, Typeable, Enum, Bounded)

instance ToJSON ExprType where
  toJSON StringT = Aeson.String "string"
  toJSON NumberT = Aeson.String "number"
  toJSON BooleanT = Aeson.String "boolean"

instance FromJSON ExprType where
  parseJSON = withText "expression type" $ \case
    "string" -> pure StringT
    "number" -> pure NumberT
    "boolean" -> pure BooleanT
    other     -> fail (toS ("Unknown expression type: " <> other))

class (ToJSON a, FromJSON a, Show a, Eq a, Typeable a) => IsValue a where
  toLiteral :: a -> Value
  default toLiteral :: ToJSON a => a -> Value
  toLiteral = toJSON

  fromLiteral :: Value -> Parser a
  default fromLiteral :: FromJSON a => Value -> Parser a
  fromLiteral = parseJSON

instance IsValue Value where
  toLiteral (Aeson.Array a) = toLiteral a
  toLiteral (Aeson.Object o) = toLiteral o
  toLiteral o = toJSON o

  fromLiteral o@(Aeson.Array _) = fromLiteralArr o
  fromLiteral (Aeson.Object _) = fail "must be a literal expression"
  fromLiteral o = fromLiteralArr o <|> parseJSON o

instance IsValue Text
instance IsValue Word8
instance IsValue Number
instance IsValue Bool

instance IsValue a => IsValue (HashMap Text a) where
  toLiteral   = toLiteralArr
  fromLiteral = fromLiteralArr

toLiteralArr  :: ToJSON a => a -> Value
toLiteralArr o = toJSON [label "literal", toJSON o]

fromLiteralArr :: FromJSON a => Value -> Parser a
fromLiteralArr = withArray "literal" $ \a ->
    if V.length a == 2 then do
      tag :: Text <- parseJSON (V.unsafeIndex a 0)
      if tag == "literal"
        then parseJSON (V.unsafeIndex a 1)
        else fail "Invalid literal"
    else fail "Invalid literal"

instance IsValue Aeson.Array where
  toLiteral   = toLiteralArr
  fromLiteral = fromLiteralArr

instance IsValue a => IsValue [a] where
  toLiteral   = toLiteralArr
  fromLiteral = fromLiteralArr

data ArrayCheck = ArrayCheck ExprType (Maybe Int)
  deriving (Show, Eq, Typeable)

type StrMap = HashMap Text



data Interpolation
  = Linear
  | Exponential Double
  | CubicBezier Double Double Double Double
  deriving (Show, Eq, Typeable)

instance ToJSON Interpolation where
  toJSON Linear = toJSON [label "linear"]
  toJSON (Exponential b) = toJSON [label "exponential", toJSON b]
  toJSON (CubicBezier a b c d) =  toJSON [label "cubic-bezier", toJSON a, toJSON b, toJSON c, toJSON d]

instance FromJSON Interpolation where
  parseJSON = withArray "interpolation" (go . V.toList)
    where
    go [tag]             | tag == label "linear"       = pure Linear
    go [tag, b]          | tag == label "exponential"  = Exponential <$> parseJSON b
    go [tag, a, b ,c ,d] | tag == label "cubic-bezier" =
      CubicBezier <$> parseJSON a <*> parseJSON b <*> parseJSON c <*> parseJSON d
    go _ = fail "Invalid interpolation"







parseValue :: Value -> Parser (Expr Value)
parseValue = parseExpr

parseExpr :: (FromJSON (Expr a), IsValue a) => Value -> Parser (Expr a)
parseExpr o = parseLit o
          <|> parseGet o
          <|> parseId o
          <|> parseCase o
          <|> parseAt o
          <|> parseCoalesce o
          <|> parseMatch o
          <|> parseStep o
          <|> parseLet o
          <|> parseVar o

parseCoalesce :: (FromJSON (Expr a), IsValue a) => Value -> Parser (Expr a)
parseCoalesce = parseOp2Args "coalesce" Coalesce

parseLit :: IsValue a => Value -> Parser (Expr a)
parseLit = fmap Lit . fromLiteral

parseId :: Value -> Parser (Expr a)
parseId = parseConst  "id" Id

parseGet :: IsValue a => Value -> Parser (Expr a)
parseGet o = parseOp2 "get" (\k v -> Get k (Just v)) o
         <|> parseOp  "get" (flip Get Nothing) o

parseAt :: IsValue a => Value -> Parser (Expr a)
parseAt = parseOp2 "at" At

parseVar :: IsValue a => Value -> Parser (Expr a)
parseVar = parseOp  "var" Var

parseCase :: (FromJSON (Expr a), IsValue a) => Value -> Parser (Expr a)
parseCase = withArray "case expression" go
  where
    go xs | V.length xs > 1
          , V.unsafeIndex xs 0 == label "case"
          , expr <- V.unsafeTail xs
          , dflt <- V.unsafeLast expr
          , cases <- V.unsafeInit expr
          = do
            cases_ <- parsePairs cases
            dflt_ <- parseJSON dflt
            pure (Case cases_ dflt_)
    go _ = fail "invalid case expression"

parseMatch :: (FromJSON (Expr a), IsValue a) => Value -> Parser (Expr a)
parseMatch = withArray "match expression" go
  where
    go xs | V.length xs > 2
          , V.unsafeIndex xs 0 == label "match"
          , expr <- V.unsafeTail xs
          , dflt <- V.unsafeLast expr
          , input <- V.unsafeHead expr
          , cases <- V.unsafeInit (V.unsafeTail expr)
          =
           (do cases_ <- parsePairs cases
               dflt_ <- parseJSON dflt
               input_ <- parseJSON @(Expr Bool) input
               pure (Match input_ cases_ dflt_))
          <|>
           (do cases_ <- parsePairs cases
               dflt_ <- parseJSON dflt
               input_ <- parseJSON @(Expr Text) input
               pure (Match input_ cases_ dflt_))
          <|>
           (do cases_ <- parsePairs cases
               dflt_ <- parseJSON dflt
               input_ <- parseJSON @(Expr Number) input
               pure (Match input_ cases_ dflt_))
          <|>
           (do cases_ <- parsePairs cases
               dflt_ <- parseJSON dflt
               input_ <- parseJSON @(Expr Value) input
               pure (Match input_ cases_ dflt_))
    go _ = fail "invalid match expression"

parseLet :: (FromJSON (Expr a), IsValue a) => Value -> Parser (Expr a)
parseLet = withArray "let expression" go
  where
    go xs | V.length xs > 1
          , V.unsafeIndex xs 0 == label "let"
          , bs <- V.unsafeInit (V.unsafeTail xs)
          , expr <- V.unsafeLast xs
          =
            (Let <$> (parseNEPairs bs :: Parser (Bindings Bool))
                 <*> parseJSON expr)
            <|>
            (Let <$> (parseNEPairs bs :: Parser (Bindings Number))
                 <*> parseJSON expr)
            <|>
            (Let <$> (parseNEPairs bs :: Parser (Bindings Text))
                 <*> parseJSON expr)
            <|>
            (Let <$> (parseNEPairs bs :: Parser (Bindings Value))
                 <*> parseJSON expr)
    go _ = fail "invalid match expression"

parseInterpolate
  :: forall a
   . (FromJSON (Expr a), IsValue a)
  => Value -> Parser (Expr a)
parseInterpolate = withArray "interpolate expression" go
  where
    go xs | V.length xs > 2
          , V.unsafeIndex xs 0 == label "interpolate"
          , ty <- V.unsafeIndex xs 1
          , input <- V.unsafeIndex xs 2
          , cases <- V.drop 3 xs
          = do
            ty_ <- parseJSON ty
            input_ <- parseJSON input
            cases_ <- parsePairs cases
            pure (Interpolate ty_ input_ cases_)
    go _ = fail "invalid interpolate expression"

parseStep
  :: forall a
   . (FromJSON (Expr a), IsValue a)
  => Value -> Parser (Expr a)
parseStep = withArray "step expression" go
  where
    go xs | V.length xs > 2
          , V.unsafeIndex xs 0 == label "step"
          , input <- V.unsafeIndex xs 1
          , out0 <- V.unsafeIndex xs 2
          , cases <- V.drop 3 xs
          = do
            input_ <- parseJSON input
            out0_ <- parseJSON out0
            cases_ <- parsePairs cases
            pure (Step input_ out0_ cases_)
    go _ = fail "invalid step expression"

parseNEPairs
  :: (FromJSON a, FromJSON b)
  => V.Vector Value -> Parser (NonEmpty (a,b))
parseNEPairs = maybe (fail "empty pairs") pure . nonEmpty
           <=< parsePairs

parsePairs
  :: (FromJSON a, FromJSON b)
  => V.Vector Value -> Parser [(a,b)]
parsePairs xs | V.length xs `mod` 2 /= 0 = fail "Odd number of pairs"
parsePairs xs = forM [0,2 .. V.length xs -1] $ \i ->
  (,) <$> parseJSON (V.unsafeIndex xs i) <*> parseJSON (V.unsafeIndex xs (i+1))


parseNumber :: (IsValue a, FromJSON (Expr a)) => Value -> Parser (Expr a)
parseNumber o = parseExpr o
            <|> parseOpArgs           "number"    Number   o
            <|> parseOpArgs           "to-number" ToNumber o
            <|> parseOp @(Expr Value) "length"    Length o
            <|> parseInterpolate o
            <|> parseOp2              "-"         Minus o
            <|> parseOp2              "*"         Mult o
            <|> parseOp2              "/"         Div  o
            <|> parseOp2              "%"         Mod  o
            <|> parseOp2              "^"         Pow  o
            <|> parseOp2              "+"         Plus  o
            <|> parseOp               "acos"      Acos  o
            <|> parseOp               "asin"      Asin  o
            <|> parseOp               "atan"      Atan  o
            <|> parseOp               "cos"       Cos   o
            <|> parseConst            "e"         E     o
            <|> parseOp               "ln"        Ln    o
            <|> parseConst            "ln2"       Ln2   o
            <|> parseOp               "log10"     Log10 o
            <|> parseOp               "log2"      Log2  o
            <|> parseOp2Args          "max"       Max   o    
            <|> parseOp2Args          "min"       Min   o    
            <|> parseConst            "pi"        Pi    o
            <|> parseOp               "sin"       Sin   o
            <|> parseOp               "sqrt"      Sqrt  o
            <|> parseOp               "tan"       Tan   o
            <|> parseConst            "zoom"      Zoom  o
            <|> parseConst            "heatmap-density" HeatmapDensity  o

parseString :: Value -> Parser (Expr Text)
parseString o = parseExpr o
            <|> parseOpArgs           "string"        String       o
            <|> parseOpArgs           "to-string"     ToString     o
            <|> parseOp @(Expr Value) "typeof"        TypeOf       o
            <|> parseConst            "geometry-type" GeometryType o
            <|> parseOp2Args          "concat"        Concat       o
            <|> parseOp               "downcase"      Downcase     o
            <|> parseOp               "upcase"        Upcase       o

parseObject :: IsValue o => Value -> Parser (Expr (StrMap o))
parseObject o = parseExpr o
            <|> parseOpArgs "object"     Object     o
            <|> parseConst  "properties" Properties o

parseColor :: Value -> Parser (Expr Color)
parseColor o = parseExpr o
           <|> parseOpArgs "to-color" ToColor o
           <|> parseInterpolate o
           <|> parseOp3  "rgb" RGB o
           <|> parseOp4  "rgba" RGBA o


parseBool :: Value -> Parser (Expr Bool)
parseBool o = parseExpr o
          <|> parseOpArgs               "boolean"    Boolean o
          <|> parseOp @(Expr Value)     "to-boolean" ToBoolean o
          <|> parseOp2                  "has"        (\k (v :: Expr (StrMap Value)) -> Has k (Just v)) o
          <|> parseOp                   "has"        (flip Has (Nothing :: Maybe (Expr (StrMap Value)))) o
          <|> parseOp2                  "!has"       (\k (v :: Expr (StrMap Value)) -> NotHas k (Just v)) o
          <|> parseOp                   "!has"       (flip NotHas (Nothing :: Maybe (Expr (StrMap Value)))) o
          <|> parseOp                   "!"          Not o
          <|> parseOp2 @(Expr Value)    "!="         NotEqual o
          <|> parseOp2 @(Expr Value)    "<"          LessThan o
          <|> parseOp2 @(Expr Value)    "<="         LessThanEq o
          <|> parseOp2 @(Expr Value)    "=="         Equal o
          <|> parseOp2 @(Expr Value)    ">"          GreaterThan o
          <|> parseOp2 @(Expr Value)    ">="         GreaterThanEq o
          <|> parseOp2Args              "all"        All o
          <|> parseOp2Args              "any"        Any o
          <|> parseOp2Args              "none"       None o
          <|> parseOpArgs @(Expr Value) "in"         In o
          <|> parseOpArgs @(Expr Value) "!in"        NotIn o

parseWith :: (Text -> [Value] -> Parser a) -> Value -> Parser a
parseWith go = withArray "expression" $ \a ->
  if V.length a > 0 then
    flip go (V.toList (V.unsafeTail a)) =<< parseJSON (V.unsafeHead a)
  else fail "empty expression"

constE :: Text -> Aeson.Value
constE name = toJSON [name]

parseConst :: Text -> Expr b -> Value -> Parser (Expr b)
parseConst tag f = parseWith go
  where
    go tag' []
      | tag==tag' = pure f
    go tag' _
      | tag==tag'  = fail (toS ("expected no args for " <> tag))
      | otherwise  = fail (toS ("unknown tag " <> tag' <> ", expected " <> tag))


op :: ToJSON a => Text -> a -> Aeson.Value
op name a = toJSON [toJSON name, toJSON a]

parseOp :: FromJSON a => Text -> (a -> b) -> Value -> Parser b
parseOp tag f = parseWith go
  where
    go tag' [x]
      | tag==tag' = f <$> parseJSON x
    go tag' _
      | tag==tag'  = fail (toS ("expected one arg for " <> tag))
      | otherwise  = fail (toS ("unknown tag " <> tag' <> ", expected " <> tag))

opArgs :: ToJSON a => Text -> a -> [a] -> Aeson.Value
opArgs name a as = toJSON (toJSON name:toJSON a:map toJSON as)

parseOpArgs :: FromJSON a => Text -> (a -> [a] -> b) -> Value -> Parser b
parseOpArgs tag f = parseWith go
  where
    go tag' (a:xs)
      | tag==tag'  = f <$> parseJSON a <*> traverse parseJSON xs
    go tag' _
      | tag==tag'  = fail (toS ("expected at least one arg for " <> tag))
      | otherwise  = fail (toS ("unknown tag " <> tag' <> ", expected " <> tag))


op2 :: (ToJSON a, ToJSON b) => Text -> a -> b -> Aeson.Value
op2 name a b = toJSON [toJSON name, toJSON a, toJSON b]

parseOp2 :: (FromJSON a, FromJSON b) => Text -> (a -> b -> c) -> Value -> Parser c
parseOp2 tag f = parseWith go
  where
    go tag' [x,y]
      | tag==tag' = f <$> parseJSON x <*> parseJSON y
    go tag' _
      | tag==tag'  = fail (toS ("expected two args for " <> tag))
      | otherwise  = fail (toS ("unknown tag " <> tag' <> ", expected " <> tag))

op2Args :: ToJSON a => Text -> a -> a -> [a] -> Aeson.Value
op2Args name a b xs = toJSON (toJSON name:toJSON a:toJSON b:map toJSON xs)

parseOp2Args :: FromJSON a => Text -> (a -> a -> [a] -> b) -> Value -> Parser b
parseOp2Args tag f = parseWith go
  where
    go tag' (a:b:xs)
      | tag==tag' = f <$> parseJSON a <*> parseJSON b <*> traverse parseJSON xs
    go tag' _
      | tag==tag'  = fail (toS ("expected at least two args for " <> tag))
      | otherwise  = fail (toS ("unknown tag " <> tag' <> ", expected " <> tag))

op3 :: (ToJSON a, ToJSON b, ToJSON c) => Text -> a -> b -> c -> Aeson.Value
op3 name a b c = toJSON [toJSON name, toJSON a, toJSON b, toJSON c]

parseOp3
  :: (FromJSON a, FromJSON b, FromJSON c)
  => Text -> (a -> b -> c -> d) -> Value -> Parser d
parseOp3 tag f = parseWith go
  where
    go tag' [x,y,z]
      | tag==tag' = f <$> parseJSON x <*> parseJSON y <*> parseJSON z
    go tag' _
      | tag==tag'  = fail (toS ("expected three args for " <> tag))
      | otherwise  = fail (toS ("unknown tag " <> tag' <> ", expected " <> tag))

op4 :: (ToJSON a, ToJSON b, ToJSON c, ToJSON d) => Text -> a -> b -> c -> d -> Aeson.Value
op4 name a b c d = toJSON [toJSON name, toJSON a, toJSON b, toJSON c, toJSON d]

parseOp4
  :: (FromJSON a, FromJSON b, FromJSON c, FromJSON d)
  => Text -> (a -> b -> c -> d -> e) -> Value -> Parser e
parseOp4 tag f = parseWith go
  where
    go tag' [x,y,z,w]
      | tag==tag' = f <$> parseJSON x <*> parseJSON y <*> parseJSON z <*> parseJSON w
    go tag' _
      | tag==tag'  = fail (toS ("expected four args for " <> tag))
      | otherwise  = fail (toS ("unknown tag " <> tag' <> ", expected " <> tag))