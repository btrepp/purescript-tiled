module Data.Tiled.File.Map.RenderOrder where

import Prelude
import Control.Monad.Error.Class (throwError)
import Data.Argonaut.Decode.Class (class DecodeJson, decodeJson)

data RenderOrder = RightDown | RightUp | LeftDown | LeftUp

derive instance eqRenderOrder :: Eq RenderOrder
instance showRenderOrder :: Show RenderOrder where
    show RightDown = "Right Down"
    show RightUp = "Right Up"
    show LeftDown = "Left Down"
    show LeftUp = "Left Up"
instance decodeJsonRenderOrder :: DecodeJson RenderOrder where
    decodeJson js =  do
        st <- decodeJson js
        case st of
            "left-up" -> pure LeftUp
            "left-down" -> pure LeftDown
            "right-down" -> pure RightDown
            "right-up" -> pure RightUp
            x -> throwError (x <> "is not a render order")
