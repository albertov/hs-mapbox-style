module Mapbox.Style.TH (
  makeUnderscoreSuffixedFields
) where

import Control.Lens
import Data.Char (toUpper)
import Data.List (dropWhileEnd)
import Language.Haskell.TH

makeUnderscoreSuffixedFields :: Name -> DecsQ
makeUnderscoreSuffixedFields = makeLensesWith underscoreSuffixedFields


underscoreSuffixedFields :: LensRules
underscoreSuffixedFields = defaultFieldRules
  & lensField .~ namer
  where
    namer :: FieldNamer
    namer _ _ n = [ MethodName (mkName cls) (mkName (field++['_'])) ]
      where
      cls = "Has" ++ capitalize field
      field = dropWhile (=='_') $ dropWhileEnd (=='_') $ nameBase n

    capitalize [] = []
    capitalize (x:xs) = toUpper x : xs
