module Data.Tiled.Raw.Map where
import Data.Maybe (Maybe)
import Data.Tiled.Raw.Tileset (Tileset)
import Data.Tiled.Raw.Layer (Layer)
import Data.Tiled.Raw.Color (Color)
import Data.Tiled.Raw.Property (Property)


data Orientation = Orthoganal | Isometric | Staggered | Hexagonal
data RenderOrder = RightDown | RightUp | LeftDown | LeftUp
data StaggerAxis  = X | Y
data StaggerIndex = Odd | Even

data Map = Map {
    backgroundColor:: Maybe Color 
    , height :: Int
    , hexSideLength :: Int
    , infinite :: Boolean
    , layers :: Array Layer
    , nextLayerId :: Int
    , nextObjectId :: Int
    , orientation :: Orientation
    , properties :: Array Property
    , renderOrder :: RenderOrder
    , staggerAxis :: StaggerAxis
    , staggerIndex :: StaggerIndex
    , tiledVersion :: String
    , tileHeight :: Int
    , tileSets :: Array Tileset
    , tileWidth :: Int
    , type :: String
    , version:: Number
    , width:: Int
}