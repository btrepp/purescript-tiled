module Data.Tiled.File.StaggerAxis where


import Prelude
import Control.Monad.Error.Class (throwError)
import Data.Argonaut.Decode.Class (class DecodeJson, decodeJson)
data StaggerAxis  = X | Y

derive instance eqStaggerAxis :: Eq StaggerAxis
instance showStaggerAxis :: Show StaggerAxis where        
    show X = "X"
    show Y = "Y"
instance decodeJsonStaggerAxis :: DecodeJson StaggerAxis where
    decodeJson js = do
        sa <- decodeJson js
        case sa of
            "X" -> pure X
            "Y" -> pure Y
            x -> throwError $ x <> "is not a stagger axis"