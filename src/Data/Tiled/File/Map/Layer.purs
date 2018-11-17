module Data.Tiled.File.Map.Layer where
import Prelude
import Data.Maybe(Maybe)
import Data.Argonaut ((.?), (.??))
import Data.Newtype (class Newtype)
import Control.Monad.Error.Class (throwError)
import Data.Argonaut.Decode.Class (class DecodeJson, decodeJson)
import Data.Tiled.File.Map.Layer.Tile (Tile)
import Data.Tiled.File.Map.Layer.ObjectGroup (ObjectGroup)
import Data.Tiled.File.Map.Layer.Image (Image)
import Data.Tiled.File.Property (Property)

-- | The layer type
-- | This can be tiles, objects, images
-- | or a further grouping of layers
data Type = Tile Tile
          | ObjectGroup ObjectGroup
          | Image Image
          | Group { layers :: Array Layer}

-- | A layer in the tiled program
newtype Layer = Layer {
    height :: Int
    , id :: Int
    , name :: String
    , offsetX :: Maybe Number
    , offsetY :: Maybe Number
    , opacity :: Int
    , properties :: Maybe (Array Property)
    , type :: Type
    , visible :: Boolean
    , width :: Int
    , x :: Int
    , y :: Int
}

derive instance newtypeLayer :: Newtype Layer _
instance decodeJsonType :: DecodeJson Type where
    decodeJson js = do
      o <- decodeJson js
      text <- o .? "type"
      case text of 
        "tilelayer" -> Tile <$> decodeJson js
        "objectgroup" -> ObjectGroup <$> decodeJson js
        "imagelayer" -> Image <$> decodeJson js
        "group" -> do
            layers <- o .? "layers"
            pure $ Group $ { layers }
        x -> throwError $  x <> " is not a layer type"
instance decodeJsonLayer :: DecodeJson Layer where
    decodeJson js = do
        o <- decodeJson js
        height <- o .? "height"
        id <- o .? "id"
        name <- o .? "name"
        offsetX <- o .?? "offsetx"
        offsetY <- o .?? "offsety"
        opacity <- o .? "opacity"
        properties <- o .?? "properties"
        _type <- decodeJson js
        visible <- o .? "visible"
        width <- o .? "width"
        x <- o .? "x"
        y <- o .? "y"
        
        pure $ Layer $ {
            height
            , id
            , name
            , offsetX
            , offsetY
            , opacity
            , properties
            , type : _type
            , visible
            , width
            , x 
            , y
        }