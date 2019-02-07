module Data.Tiled.Map 
    (Map
    , fromFiles)
    where
import Data.Tiled.Tile (Tile)
import Data.List (List)
import Data.Tiled.File.Map as MapFile
import Data.Tiled.File.Tileset as TilesetFile
import Data.Map as M
import Control.Monad.Except (Except, throwError)

-- | A fully loaded map
-- | This data structure should represent a valid map
-- | It needs to be made by combining the file formats
-- | Together. This mainly makes it much easier
-- | to render things 
type Map = 
    { height :: Int
    , width :: Int
    , tiles :: List Tile
    }



fromFiles :: MapFile.Map 
            -> M.Map String TilesetFile.Tileset 
            -> Except String Map
fromFiles mapfile
          tilesets
          =
          throwError "TODO"