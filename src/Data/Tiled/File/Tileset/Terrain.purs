module Data.Tiled.File.Tileset.Terrain where
import Prelude
import Data.Argonaut ((.?))
import Data.Argonaut.Decode.Class (decodeJson, class DecodeJson)
import Data.Newtype (class Newtype,wrap)

newtype Terrain = Terrain
    { name :: String
      ,tile :: Int
    }


derive instance newtypeTerrain :: Newtype Terrain _
derive instance eqTerrain :: Eq Terrain 
instance showTerrain :: Show Terrain where
    show (Terrain x) =  "Terrain" <> show x
instance decodeJsonTerrain :: DecodeJson Terrain where    
    decodeJson js = do
        obj <- decodeJson js
        name <- obj .? "name"
        tile <- obj .? "tile"
        pure $ wrap $ { name, tile}      