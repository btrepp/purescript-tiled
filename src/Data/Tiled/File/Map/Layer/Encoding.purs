module Data.Tiled.File.Map.Layer.Encoding where


import Prelude
import Control.Monad.Error.Class (throwError)
import Data.Argonaut.Decode.Class (class DecodeJson, decodeJson)
data Encoding = CSV | Base64
derive instance eqEncoding :: Eq Encoding
instance showEncoding :: Show Encoding where
    show CSV = "CSV"
    show Base64 = "base64"
instance decodeJsonEncoding :: DecodeJson Encoding where
    decodeJson js = do
        val <- decodeJson js
        case val of 
         "base64" -> pure Base64
         "csv" -> pure CSV
         x -> throwError $ x <> " is not a valid encoding"