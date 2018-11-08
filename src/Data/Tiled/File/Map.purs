module Data.Tiled.File.Map where

import Prelude

import Data.Argonaut (decodeJson, (.?),(.??), class DecodeJson)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, wrap)
import Data.Tiled.File.Layer (Layer)
import Data.Tiled.File.Tileset (Tileset)

data Orientation = Orthoganal | Isometric | Staggered | Hexagonal
data RenderOrder = RightDown | RightUp | LeftDown | LeftUp
derive instance eqRenderOrder :: Eq RenderOrder
instance showRenderOrder :: Show RenderOrder where
    show :: RenderOrder -> String
    show RightDown = "Right Down"
    show RightUp = "Right Up"
    show LeftDown = "Left Down"
    show LeftUp = "Left Up"
data StaggerAxis  = X | Y
data StaggerIndex = Odd | Even
type Color = String
type Property = String

type MapRecord = {
    backgroundColor:: Maybe Color 
    , height :: Int
    , hexSideLength :: Maybe Int
    , infinite :: Boolean
    , layers :: Array Layer
    , nextLayerId :: Int
    , nextObjectId :: Int
    , orientation :: Orientation
    , properties :: Array Property
    , renderOrder :: RenderOrder
    --, staggerAxis :: StaggerAxis
    --, staggerIndex :: StaggerIndex
    , tiledVersion :: String
    , tileHeight :: Int
    , tileSets :: Array Tileset
    , tileWidth :: Int
    , mapType :: String
    , version:: Number
    , width:: Int
}



newtype Map = Map MapRecord
derive instance newtypeMap :: Newtype Map _

instance decodeJsonMap :: DecodeJson Map where
    decodeJson js= do
        o <- decodeJson js
        backgroundColor <- pure Nothing
        height <- o .? "height"
        hexSideLength <- o .?? "hexsidelength"
        infinite <- o .? "infinite"
        layers <- pure mempty
        nextLayerId <- o .? "nextlayerid"
        nextObjectId <- o .? "nextobjectid"
        orientation <- o .? "orientation"
        properties <- pure mempty
        tiledVersion <- o .? "tiledversion"
        tileHeight <- o .? "tileheight"
        tileSets <- pure mempty
        tileWidth <- o .? "tilewidth"
        version <- o .? "version"
        width <- o .? "width"
        mapType <- o .? "type"
        renderOrder <- pure RightDown

        pure $ wrap $ {
            backgroundColor
            , height
            , renderOrder
            , hexSideLength
            , infinite
            , layers
            , nextLayerId
            , nextObjectId
            , orientation
            , properties
            , tiledVersion
            , tileHeight
            , tileSets
            , tileWidth 
            , mapType
            , version
            , width
        }

instance decodeJsonOrientation ::DecodeJson Orientation where
    decodeJson _ = Right Orthoganal