module Data.Tiled.File.Map 
     where

import Prelude
import Data.Argonaut (Json, decodeJson, (.:), (.:?))
import Data.Either (Either)
import Data.Maybe (Maybe(..))
import Data.Tiled.Color (Color)
import Data.Tiled.File.Layer (Layer)
import Data.Tiled.File.Property (Property)
import Data.Tiled.File.Tileset as TS
import Data.Tiled.Orientation (Orientation)
import Data.Tiled.RenderOrder (RenderOrder)
import Data.Traversable (traverse)
import Type.Data.Boolean (kind Boolean)

data Tileset = Embedded TS.Tileset
             | Reference { firstgid :: Int
                         , source :: String}
                          
type Map = 
    { backgroundColor:: Maybe Color 
    , height :: Int
    , infinite :: Boolean
    , layers :: Array Layer
    , nextLayerId :: Int
    , nextObjectId :: Int
    , orientation :: Orientation
    , properties :: Maybe (Array Property)
    , renderOrder :: RenderOrder
    , tiledVersion :: String
    , tileHeight :: Int
    , tileSets :: Array Tileset
    , tileWidth :: Int
    , type :: String
    , version:: Number
    , width:: Int
}

decodeJsonTileset :: Json -> Either String Tileset
decodeJsonTileset js = do
        o <- decodeJson js
        gid <- o .:? "firstgid"
        case gid of
            Nothing -> Embedded <$> TS.decodeJsonTileset js
            Just firstgid -> do
                source <- o .: "source"
                pure $ Reference { source, firstgid }

decodeJsonMap :: Json -> Either String Map 
decodeJsonMap js = do
        o <- decodeJson js
        backgroundColor <- o .:? "backgroundcolor"
        height <- o .: "height"
        infinite <- o .: "infinite"
        layers <- o .: "layers"
        nextLayerId <- o .: "nextlayerid"
        nextObjectId <- o .: "nextobjectid"
        orientation <- o .: "orientation"
        properties <- o .:? "properties"
        renderOrder <- o .: "renderorder"
        tiledVersion <- o .: "tiledversion"
        tileHeight <- o .: "tileheight"
        tileSetsJs <- o .: "tilesets"
        tileSets <- traverse decodeJsonTileset tileSetsJs
        tileWidth <- o .: "tilewidth"
        t <- o .: "type"
        version <- o .: "version"
        width <- o .: "width"
        pure $ 
            { backgroundColor
            , height
            , infinite
            , layers
            , nextLayerId
            , nextObjectId
            , orientation
            , properties
            , renderOrder
            , tiledVersion
            , tileHeight
            , tileSets
            , tileWidth
            , "type" : t
            , version
            , width
        }
