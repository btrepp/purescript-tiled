module Data.Tiled.File.Layer where
  
import Prelude
import Data.Argonaut (decodeJson, class DecodeJson, (.?),(.??))
import Data.Newtype (wrap, class Newtype)
import Data.Maybe (Maybe)

data Compression = Zlib | Gzip 
newtype Base64Data = Base64Data String
data LayerData =  Array Int | Compressed Base64Data
data DrawOrder = TopDown | Index
data Encoding = CSV | Base64
data Object = Object
data Opacity = Opacity
data Type = TileLayer | ObjectGroup | ImageLayer | Group

newtype Layer = Layer {
--    chunks :: Maybe (Array Chunk)
    --, compression :: Maybe Compression
    --, data :: LayerData
    --, draworder :: DrawOrder 
    --, encoding :: Encoding
     height :: Int
    , id :: Int
    , image :: Maybe String
    --, layers :: Array Layer
    , name :: String
    --, objects :: Array Object
    , offsetX :: Maybe Number
    , offsetY :: Maybe Number
    --, opacity :: Opacity
    --, properties :: Array Property
    --, transparentColor :: Color
    --, type :: Type
    , visibile :: Boolean
    , width :: Int
    , x :: Int
    , y :: Int
}
derive instance newtypeLayer :: Newtype Layer _
instance decodeJsonLayer :: DecodeJson Layer where
    decodeJson js = do
        o <- decodeJson js
        height <- o .? "height"
        id <- o .? "id"
        image <- o .?? "image"
        name <- o .? "name"
        offsetX <- o .?? "offsetx"
        offsetY <- o .?? "offsety"
        visibile <- o .? "visible"
        width <- o .? "width"
        x <- o .? "x"
        y <- o .? "y"
        
        pure $ wrap $ {
            height
            , id
            , image
            , name
            , offsetX
            , offsetY
            , visibile
            , width
            , x 
            , y
        }