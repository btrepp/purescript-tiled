module Data.Tiled.File.Map 
    (Map
    ,Tileset(..)
    ,Layer(..)
    ,TileLayer
    ,Data(..)
    ,Property
    ,decodeJsonMap
    ,externalTilesets
    ,solveTilesets
    )
     where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.Error.Class (throwError)
import Data.Argonaut (Json, decodeJson, (.:), (.:?))
import Data.Either (Either, hush, note)
import Data.Filterable (compact)
import Data.Map as M
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.Tiled.File.Compression (Compression)
import Data.Tiled.File.Encoding (Encoding)
import Data.Tiled.File.Tileset as TS
import Data.Tiled.Orientation (Orientation)
import Data.Tiled.RenderOrder (RenderOrder)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Type.Data.Boolean (kind Boolean)


-- | Tileds version of a key value pair
-- | It includes the type definition
type Property = 
    { name :: String
    , type :: String
    , value :: String }

-- | A tileset inside a map
-- | This can be a fully embedded tileset
-- | or a reference
data Tileset = Embedded { firstgid :: Int 
                         , data :: TS.Tileset }
             | Reference { firstgid :: Int
                         , source :: String}
                          
-- | The layer type
-- | This can be tiles, objects, images
-- | or a further grouping of layers
data Layer = TileLayer TileLayer

data Data = DataArray (Array Int)
          | DataString String

-- | A Data Layer in the tiled program
-- | This corresponds to tile information
type TileLayer =  {
    height :: Int
    , id :: Int
    , data :: Data
    , name :: String
    , compression :: Maybe Compression
    , encoding :: Maybe Encoding
    , offsetX :: Maybe Number
    , offsetY :: Maybe Number
    , opacity :: Int
    , properties :: Maybe (Array Property)
    , visible :: Boolean
    , width :: Int
    , x :: Int
    , y :: Int
}

-- | The Map definition
-- | Note this may not be fully fleshed out
-- | It may require the loading of any external
-- | tilesets to be useable
type Map = 
    { backgroundColor:: Maybe String
    , height :: Int
    , infinite :: Boolean
    , layers :: Array Layer
    , nextLayerId :: Int
    , nextObjectId :: Int
    , orientation :: Orientation
    , properties :: Maybe (Array Property)
    , renderOrder :: RenderOrder
    , tiledVersion :: String
    , tileHeight :: Int
    , tileSets :: Array Tileset
    , tileWidth :: Int
    , type :: String
    , version:: Number
    , width:: Int
}

-- | Given a map and the external tilesets
-- | Produce a combined workable map of firstgids
-- | to fullly fleshed tilesets
solveTilesets :: Map 
                 -> M.Map String TS.Tileset 
                 -> Either String (M.Map Int TS.Tileset)
solveTilesets map externals = 
    M.fromFoldable <$> traverse tileset map.tileSets

    where
        tileset :: Tileset -> Either String (Tuple Int TS.Tileset)
        tileset (Reference {firstgid, source}) =
            note (source <> " is missing") 
            $ Tuple firstgid
            <$> M.lookup source externals
        tileset (Embedded {firstgid, data:data'}) =
            pure $ Tuple firstgid data'

-- | Returns the names of extra tilesets that
-- | Need to be loaded
externalTilesets :: Map -> List.List String
externalTilesets m = 
    List.fromFoldable $ compact $ map value m.tileSets
    where value :: Tileset -> Maybe String
          value (Embedded _) = Nothing
          value (Reference a) = pure a.source

decodeJsonTileset :: Json -> Either String Tileset
decodeJsonTileset js = do
        o <- decodeJson js
        firstgid <- o .: "firstgid"
        sourceopt <- o .:? "source"
        case sourceopt of
            Nothing -> do 
                 data' <- TS.decodeJsonTileset js
                 pure $ Embedded { data: data', firstgid}
            Just source -> do
                pure $ Reference { source, firstgid }

decodeJsonProperty :: Json -> Either String Property
decodeJsonProperty js = do
        o <- decodeJson js
        name <- o .: "name"
        ptype <- o .: "type"
        value <- o .: "value"
        pure {name
             ,value
             ,"type" : ptype
        }

decodeJsonData :: Json -> Either String Data
decodeJsonData js = 
    note "Data is not a string or array"
        $ hush array <|> hush string
    where 
        array = DataArray <$> decodeJson js
        string = DataString <$> decodeJson js


decodeJsonLayer :: Json -> Either String Layer
decodeJsonLayer  js = do
      o <- decodeJson js
      text <- o .: "type"
      case text of 
        "tilelayer" -> TileLayer <$> decodeTileData js
        x -> throwError $  x <> " is not a layer type"

decodeTileData :: Json -> Either String TileLayer
decodeTileData js = do
        o <- decodeJson js
        height <- o .: "height"
        id <- o .: "id"
        name <- o .: "name"
        offsetX <- o .:? "offsetx"
        offsetY <- o .:? "offsety"
        opacity <- o .: "opacity"
        properties <- o .:? "properties"
        visible <- o .: "visible"
        width <- o .: "width"
        compression <- o .:? "compression"
        encoding <- o .:? "encoding"
        x <- o .: "x"
        y <- o .: "y"
        data_ <- o .: "data" >>= decodeJsonData
        
        pure $  {
            height
            , id
            , "data" : data_
            , name
            , offsetX
            , offsetY
            , opacity
            , properties
            , visible
            , compression
            , encoding
            , width
            , x 
            , y
        }

decodeJsonMap :: Json -> Either String Map 
decodeJsonMap js = do
        o <- decodeJson js
        backgroundColor <- o .:? "backgroundcolor"
        height <- o .: "height"
        infinite <- o .: "infinite"
        layers <- o .: "layers" >>= traverse decodeJsonLayer
        nextLayerId <- o .: "nextlayerid"
        nextObjectId <- o .: "nextobjectid"
        orientation <- o .: "orientation"
        properties <- o .:? "properties"
        renderOrder <- o .: "renderorder"
        tiledVersion <- o .: "tiledversion"
        tileHeight <- o .: "tileheight"
        tileSets <- o .: "tilesets" >>= traverse decodeJsonTileset
        tileWidth <- o .: "tilewidth"
        t <- o .: "type"
        version <- o .: "version"
        width <- o .: "width"
        pure $ 
            { backgroundColor
            , height
            , infinite
            , layers
            , nextLayerId
            , nextObjectId
            , orientation
            , properties
            , renderOrder
            , tiledVersion
            , tileHeight
            , tileSets
            , tileWidth
            , "type" : t
            , version
            , width
        }
