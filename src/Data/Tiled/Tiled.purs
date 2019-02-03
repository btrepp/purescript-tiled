-- | Easier functions 
-- | for getting 'renderable' data
-- | out of the files
module Data.Tiled
    (textures
    , Texture
    , Sprite )
    where

import Prelude

import Data.Either (Either(..))
import Data.List (List, fold)
import Data.Map as H
import Data.Tiled.File.Map (Map, solveTilesets)
import Data.Tiled.File.Tileset (Tileset, tiles)
import Data.Tuple (Tuple(..))

-- | A texture is based of an image in the map/tileset
-- | It has a width, height and offsets in the image
type Texture = 
    { image :: String
    , width :: Int
    , height :: Int
    , offsetX :: Int
    , offsetY :: Int
    }

-- | A sprite is something we can render
-- | It consists of a base texture to render
-- | and positions
type Sprite = 
    { texture :: Texture
    , x :: Int
    , y :: Int }


-- | Extracts all the textures from the map
-- | Requires any external tilesets to be supplied
-- | From this you should be able to take a global tile id
-- | And retrieve the texture
textures :: Map 
         -> H.Map String Tileset 
         -> Either String (H.Map Int Texture)
textures map' tilesets = do
    tileMap <- solveTilesets map' tilesets
    let tuples = (H.toUnfoldable tileMap) :: List (Tuple Int Tileset)
    let tileList = (map tiles' tuples ) 
    pure $ fold tileList

    where tiles' (Tuple k v) = tiles v k
          combine a b = b
          

-- | Loads all the sprites from the map
-- | This requires the external tilesets
-- | to have already been loaded
loadAllSprites :: Map 
                  -> H.Map String Tileset 
                  -> Either String (List Sprite)
loadAllSprites map tilesets = Left "NOT IMPLEMENTED"
