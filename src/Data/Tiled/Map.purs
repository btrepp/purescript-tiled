module Data.Tiled.Map 
    (Map
    , mapFromFiles)
    where
import Prelude

import Control.Monad.Except (Except, except, throwError)
import Data.Array (mapWithIndex)
import Data.Compactable (compact)
import Data.Either (note)
import Data.List (List, fromFoldable, concat)
import Data.Map as M
import Data.Maybe (Maybe(..))
import Data.Tiled.File.Map as MapFile
import Data.Tiled.Texture (Texture)
import Data.Tiled.Tile (Tile)
import Data.Traversable (traverse)

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

-- | Builds the map from a map file 
-- | and a loaded texture set
mapFromFiles :: MapFile.Map 
            -> M.Map Int Texture
            -> Except String Map
mapFromFiles mapfile
          textures = do
            tiles <- compact
                     <$>concat
                     <$> fromFoldable
                     <$> (traverse layer $ _.layers mapfile)
            let height = _.height mapfile * _.tileHeight mapfile
            let width = _.width mapfile * _.tileWidth mapfile
            pure {  tiles, height, width}
            where
                layer :: MapFile.Layer -> Except String (List (Maybe Tile))
                layer (MapFile.TileLayer t) = 
                    case t.data of
                        MapFile.DataString s -> 
                            throwError "compressed not supported"
                        MapFile.DataArray d -> 
                            fromFoldable 
                            <$> mapWithIndex tile
                            <$> traverse findTexture d

                    where 
                        findTexture :: Int -> Except String (Maybe Texture)
                        findTexture 0 = pure Nothing
                        findTexture i = 
                            Just
                            <$> (except 
                            $ note (show i <> " is not in the texture set")
                            $ M.lookup i textures)

                        tile :: Int -> Maybe Texture -> Maybe Tile                            
                        tile _ Nothing = Nothing
                        tile index (Just texture) = 
                            pure $ { texture
                            , height : mapfile.tileHeight
                            , width : mapfile.tileWidth
                            , flipX : false
                            , flipY : false
                            , x 
                            , y 
                            }
                            where 
                                x = (index `mod` _.height mapfile) 
                                        * _.tileHeight mapfile
                                y = (index / _.height mapfile)
                                        * _.tileWidth mapfile
