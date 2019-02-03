module Data.Tiled.File.Encoding
    (Encoding(..)
    ) 
    where

import Prelude
import Control.Monad.Error.Class (throwError)
import Data.Argonaut.Decode.Class (class DecodeJson, decodeJson)

data Encoding = Base64 | Csv

derive instance eqCompression :: Eq Encoding
instance showCompression :: Show Encoding where
    show Base64 = "Base64"
    show Csv = "Csv"

instance decodeJsonCompression :: DecodeJson Encoding where
    decodeJson js = do
        text <- decodeJson js
        case text of 
            "base64" -> pure Base64
            "csv" -> pure Csv
            x -> throwError $ x <> "is not a valid encoding"