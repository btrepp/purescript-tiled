module Data.Tiled.File.Layer where
  
import Prelude

import Control.Monad.Error.Class (throwError)
import Data.Argonaut (decodeJson, class DecodeJson, (.?), (.??))
import Data.Maybe (Maybe(..))
import Data.Newtype (wrap, class Newtype)
import Data.Tiled.File.Data (Data)
import Data.Tiled.File.Property (Property)

data Compression = Zlib | Gzip 
derive instance eqCompression :: Eq Compression 
instance showCompression :: Show Compression  where
    show Zlib = "Zlib"
    show Gzip = "Gzip"
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


type Object = String
type  Opacity = String
type Color = String
data Type = TileLayer | ObjectGroup | ImageLayer | Group
derive instance eqType :: Eq Type
instance showType :: Show Type where
    show TileLayer = "tilelayer"
    show ObjectGroup = "objectgroup"
    show ImageLayer = "imagelayer"
    show Group = "group"
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
    , transparentColor :: Color
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
        transparentColor <- pure ""
        _type <- pure TileLayer
        visible <- o .? "visible"
        width <- o .? "width"
        x <- o .? "x"
        y <- o .? "y"
        
        pure $ wrap $ {
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