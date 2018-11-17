module Data.Tiled.File.Map.Layer where
import Prelude
import Data.Maybe(Maybe(..))
import Data.Argonaut ((.?), (.??))
import Data.Newtype (class Newtype)
import Control.Monad.Error.Class (throwError)
import Data.Argonaut.Decode.Class (class DecodeJson, decodeJson)
import Data.Tiled.File.Map.Layer.Compression (Compression)
import Data.Tiled.File.Map.Layer.Data (Data)
import Data.Tiled.File.Map.Layer.Encoding (Encoding)
import Data.Tiled.File.Map.Layer.Object (Object)
import Data.Tiled.File.Map.Layer.Type (Type)
import Data.Tiled.File.Property (Property)
import Data.Tiled.File.Color (Color)

type Chunk = String

newtype Layer = Layer {
    chunks :: Maybe (Array Chunk)
    , compression :: Maybe Compression
    , data :: Data
    , encoding :: Encoding
    , height :: Int
    , id :: Int
    , name :: String
    , objects :: Array Object
    , offsetX :: Maybe Number
    , offsetY :: Maybe Number
    , opacity :: Int
    , properties :: Array Property
    , transparentColor :: Maybe Color
    , type :: Type
    , visible :: Boolean
    , width :: Int
    , x :: Int
    , y :: Int
}

derive instance newtypeLayer :: Newtype Layer _
instance decodeJsonLayer :: DecodeJson Layer where
    decodeJson js = do
        o <- decodeJson js
        chunks <- pure Nothing
        compression <- pure Nothing
        _data <- o .? "data"
        encoding <- o .? "encoding"
        height <- o .? "height"
        id <- o .? "id"
        name <- o .? "name"
        objects <- pure []
        offsetX <- o .?? "offsetx"
        offsetY <- o .?? "offsety"
        opacity <- o .? "opacity"
        properties <- pure []
        transparentColor <- o .?? "transparentcolor"
        _type <- o .? "type"
        visible <- o .? "visible"
        width <- o .? "width"
        x <- o .? "x"
        y <- o .? "y"
        
        pure $ Layer $ {
            chunks
            ,compression
            ,data : _data
            , encoding
            , height
            , id
            , name
            , objects
            , offsetX
            , offsetY
            , opacity
            , properties
            , transparentColor
            , type : _type
            , visible
            , width
            , x 
            , y
        }