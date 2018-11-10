module Data.Tiled.File.Map where

import Prelude

import Control.Monad.Error.Class (throwError)
import Data.Argonaut (decodeJson, (.?), (.??), class DecodeJson)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, wrap)
import Data.Tiled.File.Layer (Layer)
import Data.Tiled.File.Property (Property)
import Data.Tiled.File.Tileset as T

data Orientation = Orthoganal | Isometric | Staggered | Hexagonal
data RenderOrder = RightDown | RightUp | LeftDown | LeftUp
data StaggerAxis  = X | Y
data StaggerIndex = Odd | Even
data Tileset = Embedded T.Tileset
             | External { firstgid :: Int
                          , source :: String}

newtype Color = Color String

type MapRecord = {
    backgroundColor:: Maybe Color 
    , height :: Int
    , hexSideLength :: Maybe Int
    , infinite :: Boolean
    , layers :: Array Layer
    , nextLayerId :: Int
    , nextObjectId :: Int
    , orientation :: Orientation
    , properties :: Maybe (Array Property)
    , renderOrder :: RenderOrder
    , staggerAxis :: Maybe StaggerAxis
    , staggerIndex :: Maybe StaggerIndex
    , tiledVersion :: String
    , tileHeight :: Int
    , tileSets :: Array Tileset
    , tileWidth :: Int
    , mapType :: String
    , version:: Number
    , width:: Int
}

newtype Map = Map MapRecord

derive instance newtypeMap :: Newtype Map _

derive instance eqRenderOrder :: Eq RenderOrder
derive instance eqOrientation :: Eq Orientation

instance showRenderOrder :: Show RenderOrder where
    show :: RenderOrder -> String
    show RightDown = "Right Down"
    show RightUp = "Right Up"
    show LeftDown = "Left Down"
    show LeftUp = "Left Up"
instance showOrientation :: Show Orientation where        
    show :: Orientation -> String
    show Orthoganal = "Orthonganal"
    show Isometric = "Isometric"
    show Staggered = "Staggered"
    show Hexagonal = "Hexagonal"


instance decodeJsonTileset :: DecodeJson Tileset where
    decodeJson js = do 
        o <- decodeJson js
        gid <- o .?? "firstgid"
        case gid of
            Nothing -> Embedded <$> decodeJson js
            Just firstgid -> do
                source <- o .? "source"
                pure $ External { source, firstgid }
instance decodeJsonOrientation ::DecodeJson Orientation where
    decodeJson js = do
        o <- decodeJson js
        case o of
            "orthogonal" -> pure Orthoganal
            "isometric" -> pure Isometric
            "staggered" -> pure Staggered
            "hexagonal" -> pure Hexagonal
            x -> throwError $ x <> "is not an orientation"
instance decodeJsonRenderOrder :: DecodeJson RenderOrder where
    decodeJson js =  do
        st <- decodeJson js
        case st of
            "left-up" -> pure LeftUp
            "left-down" -> pure LeftDown
            "right-down" -> pure RightDown
            "right-up" -> pure RightUp
            x -> throwError (x <> "is not a render order")
instance decodeJsonStaggerAxis :: DecodeJson StaggerAxis where
    decodeJson js = do
        sa <- decodeJson js
        case sa of
            "X" -> pure X
            "Y" -> pure Y
            x -> throwError $ x <> "is not a stagger axis"
instance decodeJsonStaggerIndex :: DecodeJson StaggerIndex where                
    decodeJson js = do
      si <- decodeJson js
      case si of
        "odd" -> pure Odd
        "even" -> pure Even
        x -> throwError $ x <> "is not a stagged index"
instance decodeJsonMap :: DecodeJson Map where
    decodeJson js= do
        o <- decodeJson js
        backgroundColor <- pure Nothing
        height <- o .? "height"
        hexSideLength <- o .?? "hexsidelength"
        infinite <- o .? "infinite"
        layers <- o .? "layers"
        nextLayerId <- o .? "nextlayerid"
        nextObjectId <- o .? "nextobjectid"
        orientation <- o .? "orientation"
        properties <- o .?? "properties"
        tiledVersion <- o .? "tiledversion"
        tileHeight <- o .? "tileheight"
        tileSets <- o .? "tilesets"
        tileWidth <- o .? "tilewidth"
        version <- o .? "version"
        width <- o .? "width"
        mapType <- o .? "type"
        renderOrder <- o .? "renderorder"
        staggerAxis <- o .?? "staggeraxis"
        staggerIndex <- o .?? "staggerindex"

        pure $ wrap $ {
            backgroundColor
            , height
            , renderOrder
            , hexSideLength
            , infinite
            , layers
            , nextLayerId
            , nextObjectId
            , orientation
            , properties
            , tiledVersion
            , tileHeight
            , tileSets
            , tileWidth 
            , mapType
            , version
            , width
            , staggerAxis
            , staggerIndex
        }

