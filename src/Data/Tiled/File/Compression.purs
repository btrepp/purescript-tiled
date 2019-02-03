module Data.Tiled.File.Compression
    (Compression(..)
    ) 
    where
import Prelude
import Control.Monad.Error.Class (throwError)
import Data.Argonaut.Decode.Class (class DecodeJson, decodeJson)

data Compression = Gzip | Zlib

derive instance eqCompression :: Eq Compression
instance showCompression :: Show Compression where
    show Gzip = "Gzip"
    show Zlib = "Zlib"

instance decodeJsonCompression :: DecodeJson Compression where
    decodeJson js = do
        text <- decodeJson js
        case text of 
            "zlib" -> pure Zlib
            "gzip" -> pure Gzip
            x -> throwError $ x <> "is not a valid compression"