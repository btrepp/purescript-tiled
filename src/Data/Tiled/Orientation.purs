module Data.Tiled.Orientation where
import Prelude
import Control.Monad.Error.Class (throwError)
import Data.Argonaut.Decode.Class (class DecodeJson, decodeJson)

data Orientation = Orthoganal
                 | Isometric 
                 | Staggered 
                 | Hexagonal 

derive instance eqOrientation :: Eq Orientation
instance showOrientation :: Show Orientation where        
    show Orthoganal = "Orthonganal"
    show Isometric = "Isometric"
    show Staggered = "Staggered"
    show Hexagonal = "Hexagonal"

instance decodeJsonLayer :: DecodeJson Orientation where
    decodeJson js = do 
        text <- decodeJson js
        case text of 
            "orthogonal" -> pure Orthoganal
            "isometric" -> pure Isometric
            "staggered" -> pure Staggered
            "hexagonal" -> pure Hexagonal
            x -> throwError $ x <> "is not valid orienttaion"
