module Data.Tiled.File.Map.Layer.Type where

import Prelude
import Control.Monad.Error.Class (throwError)
import Data.Argonaut.Decode.Class (class DecodeJson, decodeJson)

data Type = TileLayer | ObjectGroup | ImageLayer | Group
derive instance eqType :: Eq Type
instance showType :: Show Type where
    show TileLayer = "tilelayer"
    show ObjectGroup = "objectgroup"
    show ImageLayer = "imagelayer"
    show Group = "group"
instance decodeJsonType :: DecodeJson Type where
    decodeJson js = do
      val <- decodeJson js
      case val of 
        "tilelayer" -> pure TileLayer
        "objectgroup" -> pure ObjectGroup
        "imagelayer" -> pure ImageLayer
        "group" -> pure Group
        x -> throwError $ x <> " is not a valid layer type"

