module Data.Tiled.File.Data where
import Prelude
import Data.Argonaut (class DecodeJson)

data Data = Uncompressed (Array Int)
            | Compressed String
instance decodeJsonData :: DecodeJson Data where
    decodeJson js = 
        pure $ Compressed "X"