module Data.Tiled.File.Tileset.Tile where
import Prelude

import Data.Argonaut ((.?))
import Data.Argonaut.Decode.Class (decodeJson, class DecodeJson)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype,wrap)
import Data.Tuple.Nested (Tuple4, tuple4)

newtype Tile = Tile 
    { id :: Int
    , terrain :: Tuple4 Int Int Int Int }

derive instance newtypeTile :: Newtype Tile _
derive instance eqTile :: Eq Tile
instance showTile :: Show Tile where
    show (Tile x) = "Tile" <> show x
instance decodeJsonTile :: DecodeJson Tile where
    decodeJson js = do
        obj <- decodeJson js
        id <- obj .? "id"
        ter <- obj .? "terrain"
        a <- value $ Array.index ter 0
        b <- value $ Array.index ter 1
        c <- value $ Array.index ter 2
        d <- value $ Array.index ter 3
        terrain <- pure $ tuple4 a b c d
        pure $ wrap $ {id,terrain}
        where
            value :: forall a. Maybe a -> Either String a         
            value (Just a) = Right a
            value Nothing = Left "Tiles incorrect"