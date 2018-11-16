module Data.Tiled.File.Map where

import Prelude

import Control.Monad.Error.Class (throwError)
import Data.Argonaut (decodeJson, (.?), (.??), class DecodeJson)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Tiled.File.Layer (Layer)
import Data.Tiled.File.Property (Property)
import Data.Tiled.File.Tileset as T

data RenderOrder = RightDown | RightUp | LeftDown | LeftUp
data StaggerAxis  = X | Y
data StaggerIndex = Odd | Even
data Orientation = Orthoganal
                 | Isometric 
                 | Staggered 
                    { axis :: StaggerAxis
                    , index :: StaggerIndex}
                 | Hexagonal 
                    { axis :: StaggerAxis                      
                    , index :: StaggerIndex
                    , hexSideLength :: Int }
data Tileset = Embedded T.Tileset
             | External { firstgid :: Int
                          , source :: String}

newtype Color = Color String

newtype Map = Map  {
    backgroundColor:: Maybe Color 
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
    , mapType :: String
    , version:: Number
    , width:: Int
}
derive instance newtypeMap :: Newtype Map _

derive instance eqRenderOrder :: Eq RenderOrder
derive instance eqOrientation :: Eq Orientation
derive instance eqStaggerAxis :: Eq StaggerAxis
derive instance eqStaggerIndex :: Eq StaggerIndex

instance showStaggerIndex :: Show StaggerIndex where
    show Odd = "Odd"
    show Even = "Even"
instance showStaggerAxis :: Show StaggerAxis where        
    show X = "X"
    show Y = "Y"
instance showRenderOrder :: Show RenderOrder where
    show RightDown = "Right Down"
    show RightUp = "Right Up"
    show LeftDown = "Left Down"
    show LeftUp = "Left Up"
instance showOrientation :: Show Orientation where        
    show Orthoganal = "Orthonganal"
    show Isometric = "Isometric"
    show (Staggered x) = "Staggered " <> show x
    show (Hexagonal x)= "Hexagonal " <> show x


instance decodeJsonTileset :: DecodeJson Tileset where
    decodeJson js = do 
        o <- decodeJson js
        gid <- o .?? "firstgid"
        case gid of
            Nothing -> Embedded <$> decodeJson js
            Just firstgid -> do
                source <- o .? "source"
                pure $ External { source, firstgid }
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
        x -> throwError $ x <> "is not a stagger index"
instance decodeJsonMap :: DecodeJson Map where
    decodeJson js= do
        o <- decodeJson js
        backgroundColor <- pure Nothing
        height <- o .? "height"
        infinite <- o .? "infinite"
        layers <- o .? "layers"
        nextLayerId <- o .? "nextlayerid"
        nextObjectId <- o .? "nextobjectid"
        orientation <- decodeOrientation o
        properties <- o .?? "properties"
        tiledVersion <- o .? "tiledversion"
        tileHeight <- o .? "tileheight"
        tileSets <- o .? "tilesets"
        tileWidth <- o .? "tilewidth"
        version <- o .? "version"
        width <- o .? "width"
        mapType <- o .? "type"
        renderOrder <- o .? "renderorder"

        pure $ Map $ {
            backgroundColor
            , height
            , renderOrder
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
        }
        where 
        
        staggerData o = do
            index <- o .? "staggerindex"
            axis <- o .? "staggeraxis"
            pure { index, axis }
        decodeOrientation o = do
            text <- o.? "orientation"
            case text of 
             "orthogonal" -> pure Orthoganal
             "isometric" -> pure Isometric
             "staggered" -> Staggered <$> staggerData o
             "hexagonal" -> do
                index <- _.index <$>staggerData o
                axis <- _.axis <$> staggerData o
                hexSideLength <- o .? "hexsidelength"
                pure $ Hexagonal {index,axis,hexSideLength}
             x -> throwError $ x <> "is not a orientation"

