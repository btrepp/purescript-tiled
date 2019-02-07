module Data.Tiled.Texture
    (Texture,texturesFromFiles)
    where

import Prelude

import Control.Monad.Except (Except, except)
import Data.Array ((..))
import Data.Either (note)
import Data.List (List,concat)
import Data.List as List
import Data.Map as M
import Data.Tiled.File.Map as MapFile
import Data.Tiled.File.Tileset as TilesetFile
import Data.Tiled.Image (Image)
import Data.Traversable (traverse)
import Data.TraversableWithIndex (traverseWithIndex)
import Data.Tuple (Tuple(..))

-- | A texture is based of an image in the map/tileset
-- | It has a width, height and offsets in the image
type Texture = 
    { image :: Image
    , width :: Int
    , height :: Int
    , offsetX :: Int
    , offsetY :: Int
    }

-- | Given a map and the external tilesets
-- | Produce a combined workable map of firstgids
-- | to fullly fleshed tilesets
-- | Will fail if a tileset isn't provided
-- | should always pass on embedded tilesets
allTilesets :: MapFile.Map 
            -> M.Map String TilesetFile.Tileset
            -> Except String (M.Map Int TilesetFile.Tileset)
allTilesets map externals =
    M.fromFoldable <$> traverse tileset (_.tileSets map)

    where 
        tileset :: MapFile.Tileset 
                -> Except String (Tuple Int TilesetFile.Tileset)
        tileset (MapFile.Reference {firstgid, source}) = 
            except 
            $ note (source <> " is missing")
            $ Tuple firstgid                
            <$> M.lookup source externals
        tileset (MapFile.Embedded {firstgid, data:data'}) = 
            pure $ Tuple firstgid data'    


-- | Extracts all the textures from the map
-- | Requires any external tilesets to be supplied
-- | From this you should be able to take a global tile id
-- | And retrieve the texture
texturesFromFiles :: MapFile.Map
                  -> M.Map String TilesetFile.Tileset
                  -> M.Map String Image
                  -> Except String (M.Map Int Texture)
texturesFromFiles mapfile tilesetfiles images = do
    tilesets <- allTilesets mapfile tilesetfiles
    lists <- M.values <$> traverseWithIndex tilesetFile tilesets
    pure $ M.fromFoldable $ concat lists

    where 
        tilesetFile :: Int 
                -> TilesetFile.Tileset 
                -> Except String (List (Tuple Int Texture))
        tilesetFile globalId tilesetfile = do
            image <- except 
                     $ note (tilesetfile.image <> " missing")
                     $ M.lookup tilesetfile.image images
            
            pure $ concat $ map (perRow image) rows

            where 
                width = tilesetfile.tileWidth
                height = tilesetfile.tileHeight
                columns = List.fromFoldable $ 0..tilesetfile.columns
                rows = List.fromFoldable 
                        $ 0..(tilesetfile.tileCount/tilesetfile.columns)
                tileId row column = row*tilesetfile.columns + column + globalId
                perRow image rowInt = 
                    map (makeTexture image rowInt) columns
                makeTexture image row column = 
                    Tuple (row*tilesetfile.columns + column + globalId)
                        { image
                        , width
                        , height
                        , offsetX : column * width
                        , offsetY : row* height
                        }
