module Data.Tiled.File.Map.Layer.Object where

import Prelude
import Control.Monad.Error.Class (throwError)
import Data.Argonaut.Decode.Class (class DecodeJson, decodeJson)

type Object = String