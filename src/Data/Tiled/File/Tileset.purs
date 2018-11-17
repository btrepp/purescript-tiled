module Data.Tiled.File.Tileset where
  
import Prelude

import Data.Argonaut (class DecodeJson, decodeJson, (.?))
import Data.Newtype (class Newtype, wrap)
import Data.Tuple.Nested (Tuple4,tuple4)
import Data.Either(Either(..))
import Data.Maybe(Maybe(..))
import Data.Array as Array

newtype Terrain = Terrain
    { name :: String
      ,tile :: Int
    }
newtype Tile = Tile 
    { id :: Int
    , terrain :: Tuple4 Int Int Int Int }

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

derive instance newtypeTerrain :: Newtype Terrain _
derive instance newtypeTile :: Newtype Tile _
derive instance newtypeTileset :: Newtype Tileset _

derive instance eqTerrain :: Eq Terrain 
derive instance eqTile :: Eq Tile
derive instance eqTileset :: Eq Tileset

instance showTile :: Show Tile where
    show (Tile x) = "Tile" <> show x
instance showTerrain :: Show Terrain where
    show (Terrain x) =  "Terrain" <> show x
instance showTileset :: Show Tileset where
    show (Tileset t) = "Tileset" <> show t

instance decodeJsonTerrain :: DecodeJson Terrain where    
    decodeJson js = do
        obj <- decodeJson js
        name <- obj .? "name"
        tile <- obj .? "tile"
        pure $ wrap $ { name, tile}            
instance decodeJsonTile :: DecodeJson Tile where
    decodeJson js = do
        obj <- decodeJson js
        id <- obj .? "id"
        ter <- obj .? "terrain"
        a <- value $ Array.index ter 0
        b <- value $ Array.index ter 1
        c <- value $ Array.index ter 2
        d <- value $ Array.index ter 3
        terrain <- pure $ tuple4 a b c d
        pure $ wrap $ {id,terrain}
        where
            value :: forall a. Maybe a -> Either String a         
            value (Just a) = Right a
            value Nothing = Left "Tiles incorrect"
              

type Version = String 
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