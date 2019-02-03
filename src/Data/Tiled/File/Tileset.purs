module Data.Tiled.File.Tileset 
    (Tile
    , Tileset
    , Terrain 
    , decodeJsonTileset
    , tiles)
    where
  
import Prelude

import Data.Argonaut (Json, decodeJson, (.:), (.:?))
import Data.Array (concat, (..))
import Data.Either (Either)
import Data.Map (Map, fromFoldable)
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))

type Tile =
    { id :: Int
    , terrain :: Array Int }

type Terrain =
    { name :: String
      ,tile :: Int
    }

type Tileset = 
      { columns :: Int
      , image :: String
      , imageHeight :: Int
      , imageWidth :: Int
      , margin :: Int
      , name :: String
      , spacing :: Int
      , terrains :: Maybe (Array Terrain)
      , tileCount :: Int
      , tiledVersion :: String
      , tileHeight :: Int
      , tiles :: Maybe (Array Tile)
      , tileWidth :: Int
      , type :: String
      , version :: Number
    }

-- | Extracts the tiles from the tileset
-- | Converting into the global id format
tiles :: Tileset -> Int -> Map Int {image:: String
                                   ,height :: Int
                                   ,width :: Int
                                   ,offsetX :: Int
                                   ,offsetY :: Int}
tiles tileset firstgid = 
    let width = tileset.tileWidth
        height = tileset.tileHeight
        image = tileset.image
        columns = 0..tileset.columns
        rows = 0..(tileset.tileCount/tileset.columns)
        tileId row column = row*tileset.columns + column + firstgid
        makeTile row column = 
            Tuple (tileId row column)
                {image
                ,height
                ,width
                ,offsetX : column * width
                ,offsetY : row * height
                }
        perRow rowInt = 
            map (makeTile rowInt) columns
    in
        fromFoldable $ concat $  map perRow rows

decodeJsonTile :: Json -> Either String Tile
decodeJsonTile js = do
    o <- decodeJson js
    id <- o .: "id"
    terrainArray <- o .: "terrain"
    terrain <- traverse decodeJson terrainArray
    pure { id
         , terrain }


decodeJsonTerrain :: Json -> Either String Terrain    
decodeJsonTerrain js = do
    o <- decodeJson js
    name <- o .: "name"
    tile <- o .: "tile"
    pure { name
         , tile }

decodeJsonTileset :: Json -> Either String Tileset
decodeJsonTileset js = do
    o <- decodeJson js
    columns <- o .: "columns"
    image <- o .: "image"
    imageHeight <- o .: "imageheight"
    imageWidth <- o .: "imagewidth"
    margin <- o .: "margin"
    name <- o .: "name"
    spacing <- o .: "spacing"
    terrainsArray <- o .:? "terrains"
    terrains <- case terrainsArray of
                    Just x -> Just <$> traverse decodeJsonTerrain x
                    Nothing -> pure Nothing
    tileCount <- o .: "tilecount"
    tiledVersion <- o .: "tiledversion"
    tileHeight <- o .: "tileheight"
    tilesArray <- o .:? "tiles"
    tiles <- case tilesArray of
               Just x -> Just <$> traverse decodeJsonTile x
               Nothing -> pure Nothing 
    tileWidth <- o .: "tilewidth"
    type_ <- o .: "type"
    version <- o .: "version"
    pure { columns
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
         , tiles
         , tileWidth
         , "type" : type_
         , version
    }    
