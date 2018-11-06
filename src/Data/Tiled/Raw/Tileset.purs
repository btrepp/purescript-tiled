module Data.Tiled.Raw.Tileset where
  
import Prelude

import Data.Argonaut (class DecodeJson, decodeJson, (.?))


newtype Terrain = Terrain 
    { name :: String
      , tile :: Int
    }
instance eqTerrain :: Eq Terrain where
    eq (Terrain a) (Terrain b ) = a == b
instance showTerrain :: Show Terrain where
    show (Terrain x) = show x
    
type Version = String 

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
    }

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
    terrains <- pure mempty

    pure $ Tileset {   columns
                     , image
                     , imageHeight
                     , imageWidth
                     , margin
                     , name
                     , spacing 
                     , terrains
                     , tileCount
                     , tiledVersion
                     , tileHeight }