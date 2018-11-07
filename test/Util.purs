module Test.Tiled.Util where

import Prelude
import Effect.Aff(Aff,throwError,error)
import Effect.Class (liftEffect)
import Data.Either(Either(..))
import Data.Argonaut (decodeJson, class DecodeJson)
import Data.Argonaut.Parser (jsonParser)
import Node.Encoding (Encoding(..))
import Node.FS.Sync as FS

-- | Loads a file for a type that supports decodeJson
loadJsonFile :: forall a. DecodeJson a => String -> Aff a
loadJsonFile path = do
  file <- liftEffect $ FS.readTextFile ASCII path
  js <- json file
  case decodeJson js of
    Left x -> throwError (error x)
    Right x -> pure x
  where json text =
          case jsonParser text of
          Left x -> throwError (error x)
          Right x -> pure x