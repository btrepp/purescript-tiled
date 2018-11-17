module Data.Tiled.File.Tileset where
  
import Prelude
import Data.Argonaut (class DecodeJson, decodeJson, (.?))
import Data.Newtype (class Newtype, wrap)
import Data.Tiled.File.Tileset.Terrain(Terrain)
import Data.Tiled.File.Tileset.Tile(Tile)
import Data.Tiled.File.Tileset.Version(Version)

newtype Tileset = Tileset
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
      , tiles :: Array Tile
      , tileWidth :: Int
      , typeTileset :: String
      , version :: Number
    }

derive instance newtypeTileset :: Newtype Tileset _
derive instance eqTileset :: Eq Tileset
instance showTileset :: Show Tileset where
    show (Tileset t) = "Tileset" <> show t

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
    tiles <- obj .? "tiles"

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
                     , version
                     , tiles}