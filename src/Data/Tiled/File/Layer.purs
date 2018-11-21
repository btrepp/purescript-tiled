module Data.Tiled.File.Layer where
import Prelude

import Control.Monad.Error.Class (throwError)
import Data.Argonaut ((.:), (.:?))
import Data.Argonaut.Decode.Class (class DecodeJson, decodeJson)
import Data.Maybe (Maybe)
import Data.Tiled.File.Property (Property)

-- | The layer type
-- | This can be tiles, objects, images
-- | or a further grouping of layers
data Layer = TileLayer TileLayerData

-- | A layer in the tiled program
newtype TileLayerData = TileLayerData {
    height :: Int
    , id :: Int
    , name :: String
    , offsetX :: Maybe Number
    , offsetY :: Maybe Number
    , opacity :: Int
    , properties :: Maybe (Array Property)
    , visible :: Boolean
    , width :: Int
    , x :: Int
    , y :: Int
}

instance decodeJsonType :: DecodeJson Layer where
    decodeJson js = do
      o <- decodeJson js
      text <- o .: "type"
      case text of 
        "tilelayer" -> TileLayer <$> decodeJson js
        x -> throwError $  x <> " is not a layer type"

instance decodeTileLayerData :: DecodeJson TileLayerData where
    decodeJson js = do
        o <- decodeJson js
        height <- o .: "height"
        id <- o .: "id"
        name <- o .: "name"
        offsetX <- o .:? "offsetx"
        offsetY <- o .:? "offsety"
        opacity <- o .: "opacity"
        properties <- o .:? "properties"
        visible <- o .: "visible"
        width <- o .: "width"
        x <- o .: "x"
        y <- o .: "y"
        
        pure $ TileLayerData $ {
            height
            , id
            , name
            , offsetX
            , offsetY
            , opacity
            , properties
            , visible
            , width
            , x 
            , y
        }