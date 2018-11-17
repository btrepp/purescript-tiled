module Data.Tiled.File.Map.Layer.Tile where

import Prelude
import Data.Argonaut ((.?))
import Data.Argonaut.Decode.Class (class DecodeJson, decodeJson)
import Data.Tiled.File.Map.Layer.Compression (Compression)
import Data.Tiled.File.Map.Layer.Data (Data)
import Data.Tiled.File.Map.Layer.Encoding (Encoding)
import Data.Maybe (Maybe(..))

type Chunk = String

newtype Tile = Tile {
    chunks :: Maybe (Array Chunk)
    ,compression :: Maybe Compression
    ,data :: Data
    ,encoding :: Encoding
}
instance decodeJsonTile :: DecodeJson Tile where
    decodeJson js = do
        o <- decodeJson js
        chunks <- pure Nothing
        compression <- pure Nothing
        data'  <- o .? "data"
        encoding <- o .? "encoding"
        pure $ Tile $ {
            chunks
            , compression
            , data : data'
            , encoding
        }