module Data.Tiled.File.Map.Orientation where

import Prelude

import Control.Monad.Error.Class (throwError)
import Data.Argonaut ((.?))
import Data.Argonaut.Decode.Class (class DecodeJson, decodeJson)
import Data.Tiled.File.Map.StaggerAxis (StaggerAxis)
import Data.Tiled.File.Map.StaggerIndex (StaggerIndex)
data Orientation = Orthoganal
                 | Isometric 
                 | Staggered 
                    { axis :: StaggerAxis
                    , index :: StaggerIndex}
                 | Hexagonal 
                    { axis :: StaggerAxis                      
                    , index :: StaggerIndex
                    , hexSideLength :: Int }

derive instance eqOrientation :: Eq Orientation
instance showOrientation :: Show Orientation where        
    show Orthoganal = "Orthonganal"
    show Isometric = "Isometric"
    show (Staggered x) = "Staggered " <> show x
    show (Hexagonal x)= "Hexagonal " <> show x

instance decodeJsonLayer :: DecodeJson Orientation where
    decodeJson js = do 
        o <- decodeJson js
        text <- o .? "orientation"
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
        where 
            staggerData o = do
                index <- o .? "staggerindex"
                axis <- o .? "staggeraxis"
                pure { index, axis }