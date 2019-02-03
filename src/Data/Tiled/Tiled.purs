-- | Easier functions 
-- | for getting 'renderable' data
-- | out of the files
module Data.Tiled
    where

import Prelude
import Data.List(List)
import Data.Map as H
import Data.Either (Either(..))
import Data.Tiled.File.Map(Map)
import Data.Tiled.File.Tileset(Tileset)

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
textures map tilesets = 
    Left "NOT IMPLEMENTED"         

-- | Loads all the sprites from the map
-- | This requires the external tilesets
-- | to have already been loaded
loadAllSprites :: Map 
                  -> H.Map String Tileset 
                  -> Either String (List Sprite)
loadAllSprites map tilesets = Left "NOT IMPLEMENTED"
