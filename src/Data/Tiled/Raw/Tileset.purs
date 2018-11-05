module Data.Tiled.Raw.Tileset where
  
import Data.Argonaut.Core(Json)
import Data.Either (Either(..))
data Tileset = Tileset {
    columns :: Int
}

parseTileSet :: Json -> Either String Tileset 
parseTileSet json = 
    Right tiles 
    where 
        tiles = Tileset { columns : 2 }