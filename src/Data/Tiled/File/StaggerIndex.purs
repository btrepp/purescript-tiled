module Data.Tiled.File.StaggerIndex where

import Prelude
import Control.Monad.Error.Class (throwError)
import Data.Argonaut.Decode.Class (class DecodeJson, decodeJson)

data StaggerIndex = Odd | Even
derive instance eqStaggerIndex :: Eq StaggerIndex
instance showStaggerIndex :: Show StaggerIndex where
    show Odd = "Odd"
    show Even = "Even"
instance decodeJsonStaggerIndex :: DecodeJson StaggerIndex where                
    decodeJson js = do
      si <- decodeJson js
      case si of
        "odd" -> pure Odd
        "even" -> pure Even
        x -> throwError $ x <> "is not a stagger index"