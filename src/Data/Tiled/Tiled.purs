-- | Easier functions 
-- | for getting 'renderable' data
-- | out of the files
module Data.Tiled(
     module Map
     , module Texture
     , module Tile
     )
    where

import Data.Tiled.Tile (Tile) as Tile
import Data.Tiled.Texture (Texture) as Texture
import Data.Tiled.Map (Map) as Map

-- | Loads all the sprites from the map
-- | This requires the external tilesets
-- | to have already been loaded
-- tiles :: Map 
--                   -> H.Map String Tileset 
--                   -> Except String (List Tile)
-- tiles map tilesets = do
--     textures' <- textures map tilesets
--     tiles' <- MapFile.tiles map
--     traverse (findTile textures') tiles'

--     where 
--         note' :: forall a. String -> Maybe a -> Except String a
--         note' text value = except $ note text value

--         findTile :: _ -> _ -> Except String Tile
--         findTile textureMap
--                  ({height,width,flipX,flipY,globalId})
--                   = do
--                 texture <- note' ("missing" <> globalId)
--                             $ H.lookup globalId textureMap
--                 pure { height,width,flipX,flipY,texture,x:0,y:0}


