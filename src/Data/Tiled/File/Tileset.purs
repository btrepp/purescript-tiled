module Data.Tiled.File.Tileset where
  
import Prelude

import Data.Argonaut (class DecodeJson, decodeJson, (.?))
import Data.Newtype (class Newtype, wrap)

type TerrainRecord = 
    { name :: String
      ,tile :: Int
    }
newtype Terrain = Terrain TerrainRecord
derive instance eqTerrain :: Eq Terrain 
derive instance newtypeTerrain :: Newtype Terrain _
instance showTerrain :: Show Terrain where
    show (Terrain x) = show x
instance decodeJsonTerrain :: DecodeJson Terrain where    
    decodeJson js = do
        obj <- decodeJson js
        name <- obj .? "name"
        tile <- obj .? "tile"
        pure $ wrap $ { name, tile}            
    
type Version = String 
type TilesetRecord =
    { columns :: Int
      , image :: String
      , imageHeight :: Int
      , imageWidth :: Int
      , margin :: Int
      , name :: String
      , spacing :: Int
      , terrains :: Array Terrain
      , tileCount :: Int
      , tiledVersion :: Version
      , tileHeight :: Int
      -- tiles
      , tileWidth :: Int
      , typeTileset :: String
      , version :: Number
    }
newtype Tileset = Tileset TilesetRecord

derive instance newtypeTileset :: Newtype Tileset _
instance showTileset :: Show Tileset where
    show (Tileset t) = show t
instance eqTileset :: Eq Tileset where
    eq (Tileset t) (Tileset t2) = t == t2
instance decodeTileSet :: DecodeJson Tileset where
  decodeJson json = do
    obj <- decodeJson json
    columns <- obj .? "columns"
    image <- obj .? "image"
    imageHeight <- obj .? "imageheight"
    imageWidth <- obj .? "imagewidth"
    margin <- obj .? "margin"
    name <- obj .? "name"
    spacing <- obj .? "spacing"
    tileCount <- obj .? "tilecount"
    tiledVersion <- obj .? "tiledversion"
    tileHeight <- obj .? "tileheight"
    terrains <-  obj .? "terrains"
    typeTileset <- obj .? "type"
    tileWidth <- obj .? "tilewidth"
    version <- obj .? "version"

    pure $ wrap {   columns
                     , image
                     , imageHeight
                     , imageWidth
                     , margin
                     , name
                     , spacing 
                     , terrains
                     , tileCount
                     , tiledVersion
                     , tileHeight 
                     , typeTileset
                     , tileWidth
                     , version}