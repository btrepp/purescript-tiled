module Data.Tiled.File.Map.Layer.Compression where

import Prelude
import Control.Monad.Error.Class (throwError)
import Data.Argonaut.Decode.Class (class DecodeJson, decodeJson)

data Compression = Zlib | Gzip 
derive instance eqCompression :: Eq Compression 
instance showCompression :: Show Compression  where
    show Zlib = "Zlib"
    show Gzip = "Gzip"