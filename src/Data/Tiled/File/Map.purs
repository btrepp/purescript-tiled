module Data.Tiled.File.Map 
     where

import Prelude

import Data.Argonaut (decodeJson, (.?), (.??), class DecodeJson)
import Data.List (List, fromFoldable, mapMaybe)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype,unwrap)
import Data.Tiled.File.Color (Color)
import Data.Tiled.File.Map.Layer (Layer)
import Data.Tiled.File.Map.Orientation (Orientation)
import Data.Tiled.File.Map.RenderOrder (RenderOrder)
import Data.Tiled.File.Property (Property)
import Data.Tiled.File.Tileset as T

data Tileset = Embedded T.Tileset
             | External { firstgid :: Int
                          , source :: String}
newtype Map = Map  {
    backgroundColor:: Maybe Color 
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
    , mapType :: String
    , version:: Number
    , width:: Int
}
derive instance newtypeMap :: Newtype Map _

instance decodeJsonTileset :: DecodeJson Tileset where
    decodeJson js = do 
        o <- decodeJson js
        gid <- o .?? "firstgid"
        case gid of
            Nothing -> Embedded <$> decodeJson js
            Just firstgid -> do
                source <- o .? "source"
                pure $ External { source, firstgid }


instance decodeJsonMap :: DecodeJson Map where
    decodeJson js= do
        o <- decodeJson js
        backgroundColor <- pure Nothing
        height <- o .? "height"
        infinite <- o .? "infinite"
        layers <- o .? "layers"
        nextLayerId <- o .? "nextlayerid"
        nextObjectId <- o .? "nextobjectid"
        orientation <- decodeJson js 
        properties <- o .?? "properties"
        tiledVersion <- o .? "tiledversion"
        tileHeight <- o .? "tileheight"
        tileSets <- o .? "tilesets"
        tileWidth <- o .? "tilewidth"
        version <- o .? "version"
        width <- o .? "width"
        mapType <- o .? "type"
        renderOrder <- o .? "renderorder"

        pure $ Map $ {
            backgroundColor
            , height
            , renderOrder
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

-- | Gets the paths of external tilesets
-- | From the map
-- | These would be required to do anything with the map
externalTileSets :: Map -> List String
externalTileSets m = 
    mapMaybe source 
    $ fromFoldable 
    $_.tileSets 
    $ unwrap m
    where
        source  :: Tileset -> Maybe String
        source (Embedded _ ) = Nothing
        source (External e) = pure e.source
