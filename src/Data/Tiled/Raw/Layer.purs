module Data.Tiled.Raw.Layer where
  
import Data.Maybe (Maybe)
import Data.Tiled.Raw.Property (Property) 
import Data.Tiled.Raw.Color (Color)
data Compression = Zlib | Gzip 
newtype Base64Data = Base64Data String
data LayerData =  Array Int | Compressed Base64Data
data DrawOrder = TopDown | Index
data Encoding = CSV | Base64
data Chunk = Void
data Object = Object
data Opacity = Opacity
data Type = TileLayer | ObjectGroup | ImageLayer | Group

data Layer = Layer {
    chunks :: Maybe (Array Chunk)
    , compression :: Maybe Compression
    , data :: LayerData
    , draworder :: DrawOrder 
    , encoding :: Encoding
    , height :: Int
    , id:: Int
    , image:: String
    , layers :: Array Layer
    , name:: String
    , objects :: Array Object
    , offsetX :: Number
    , offsetY :: Number
    , opacity :: Opacity
    , properties :: Array Property
    , transparentColor :: Color
    , type :: Type
    , visibile :: Boolean
    , width :: Int
    , x :: Int
    , y :: Int
}