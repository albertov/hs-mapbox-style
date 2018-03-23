{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Mapbox.TestUtil (
  jsonProp
, laxJsonProp
) where

import           Data.Aeson (ToJSON(..), FromJSON, fromJSON, encode, eitherDecode)
import           Data.Aeson.Types as AT (Result)
import           Data.Typeable 
import           Test.Hspec.QuickCheck
import           Test.Hspec.Core.Spec
import           Test.Hspec
import           Test.QuickCheck

jsonProp
  :: forall p a.
   ( Show a
   , Eq a
   , ToJSON a
   , FromJSON a
   , Arbitrary a
   , Typeable a
   )
  => p a
  -> SpecWith ()
jsonProp _ =
  prop ("decode (encode a) = Just a (" ++ show (typeOf (undefined :: a)) ++ ")") $
  \(a :: a) ->
    let encoded = encode a
        decoded = eitherDecode encoded
    in decoded `shouldBe` Right a

laxJsonProp
  :: forall p a.
   ( Show a
   , Eq a
   , ToJSON a
   , FromJSON a
   , Arbitrary a
   , Typeable a
   )
  => p a
  -> SpecWith ()
laxJsonProp _ =
  prop ("toJSON decoded = encoded (" ++ show (typeOf (undefined :: a)) ++ ")") $
  \(a :: a) ->
    let encoded = toJSON a
        decoded = fromJSON encoded :: AT.Result a
    in counterexample (show (encoded, fmap toJSON decoded)) $
       fmap toJSON decoded `shouldBe` pure encoded