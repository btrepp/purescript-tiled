module Data.Tiled.File.Map.Layer.Image where

import Control.Monad.Error.Class (throwError)
import Data.Maybe (Maybe)
import Data.Argonaut.Decode.Class (class DecodeJson)
import Data.Tiled.File.Color (Color)
newtype Image = Image {
    image :: String
    , transparentColor :: Maybe Color
}

instance decodeJsonImage :: DecodeJson Image where
    decodeJson _ = throwError "cant decode image layers"