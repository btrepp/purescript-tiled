module Data.Tiled.File.Map 
    (Map
    ,Tileset
    ,Layer
    ,Property
    ,decodeJsonMap
    ,externalTilesets
    )
     where

import Prelude

import Control.Monad.Error.Class (throwError)
import Data.Argonaut (Json, decodeJson, (.:), (.:?))
import Data.Either (Either,hush,note)
import Data.Filterable (compact)
import Data.Map as M
import Data.Maybe (Maybe(..))
import Data.Tiled.File.Tileset as TS
import Data.Tiled.Orientation (Orientation)
import Data.Tiled.RenderOrder (RenderOrder)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Control.Alt ((<|>))
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
data Tileset = Embedded TS.Tileset
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

externalTilesets :: Map -> M.Map Int String
externalTilesets m = 
    M.fromFoldable $ compact $ map toTuple m.tileSets
    where toTuple :: Tileset -> Maybe (Tuple Int String)
          toTuple (Embedded _) = Nothing
          toTuple (Reference a) = Just $ Tuple a.firstgid a.source

decodeJsonTileset :: Json -> Either String Tileset
decodeJsonTileset js = do
        o <- decodeJson js
        gid <- o .:? "firstgid"
        case gid of
            Nothing -> Embedded <$> TS.decodeJsonTileset js
            Just firstgid -> do
                source <- o .: "source"
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
