module Test.Tiled.Util where

import Prelude

import Data.Argonaut (decodeJson, class DecodeJson)
import Data.Argonaut.Parser (jsonParser)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Data.Tiled.File.Map (Map)
import Data.Tiled.File.Tileset (Tileset)
import Effect.Aff (Aff, throwError, error)
import Effect.Class (liftEffect)
import Node.Encoding (Encoding(..))
import Node.FS.Sync as FS
import Test.Unit (TestSuite, test)
import Test.Unit.Assert as Assert

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

desertMap :: Aff Map
desertMap = loadJsonFile "maps/desert.json"

desertTileset :: Aff Tileset
desertTileset = loadJsonFile "maps/desert_tileset.json"

failMaybe :: forall a. Maybe a -> Aff a
failMaybe (Just a) = pure a
failMaybe Nothing = throwError (error "Failed to get property")

-- | Runs a effect, then tests if the field matches
testField :: forall a b m . Eq a =>
                            Show a =>
                            Newtype m b =>
                            Aff m
                            -> String 
                            -> (b -> a) 
                            -> a 
                            -> TestSuite
testField load name field expected = 
  test name do
    (unwrap >>> field) <$> load >>= Assert.equal expected