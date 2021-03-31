{-# LANGUAGE NoImplicitPrelude #-}
module Mapbox.Style.Common (
  parsePairs
, parseNEPairs
, failT
, prop
) where

import Data.Aeson
import Data.Aeson.Types (Parser, Pair)
import qualified Data.Vector as V
import Protolude
import Prelude (MonadFail(fail))

failT :: MonadFail m => Text -> m a
failT = fail . toS
{-# INLINE failT #-}

parseNEPairs
  :: (FromJSON a, FromJSON b)
  => V.Vector Value -> Parser (NonEmpty (a,b))
parseNEPairs = maybe (fail "empty pairs") pure . nonEmpty
           <=< parsePairs
{-# INLINEABLE parseNEPairs #-}

parsePairs
  :: (FromJSON a, FromJSON b)
  => V.Vector Value -> Parser [(a,b)]
parsePairs xs | V.length xs `mod` 2 /= 0 = fail "Odd number of pairs"
parsePairs xs = forM [0,2 .. V.length xs -1] $ \i ->
  (,) <$> parseJSON (V.unsafeIndex xs i) <*> parseJSON (V.unsafeIndex xs (i+1))
{-# INLINEABLE parsePairs #-}

prop :: ToJSON a => Text -> Maybe a -> Maybe Pair
prop k = fmap (k .=)
{-# INLINE prop #-}
