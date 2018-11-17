module Data.Tiled.File.Map.Layer.ObjectGroup where

import Control.Monad.Error.Class (throwError)
import Data.Argonaut.Decode.Class (class DecodeJson)
import Data.Tiled.File.Map.Layer.DrawOrder (DrawOrder)
import Data.Tiled.File.Map.Layer.Object (Object)

newtype ObjectGroup = ObjectGroup {
    objects :: Array Object
    ,drawOrder :: DrawOrder
}
instance decodeJsonObjectGroup :: DecodeJson ObjectGroup where
    decodeJson _ = throwError "Cant decode object groups"