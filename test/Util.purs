module Test.Tiled.Util where

import Prelude

import Data.Argonaut (Json)
import Data.Argonaut.Parser (jsonParser)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Tiled.File.Map (Map, decodeJsonMap)
import Data.Tiled.File.Tileset (Tileset, decodeJsonTileset)
import Effect (Effect)
import Effect.Aff (Aff, throwError, error)
import Effect.Class (liftEffect)
import Effect.Exception (throw)
import Node.Encoding (Encoding(..))
import Node.FS.Sync as FS
import Test.Unit (TestSuite, test)
import Test.Unit.Assert as Assert

-- | Loads a file for a type that supports decodeJson
loadJsonFile ::  String -> Effect Json
loadJsonFile path = do
  text <- FS.readTextFile ASCII path
  js <- case jsonParser text of
          Left x -> throw x
          Right x -> pure x
  pure js 

errorEither :: forall a. Either String a -> Effect a
errorEither (Right x) = pure x
errorEither (Left s) = throw s

desertMap :: Effect Map
desertMap = loadJsonFile "maps/desert.json"
            >>= (decodeJsonMap >>> errorEither)

desertTileset :: Effect Tileset
desertTileset = loadJsonFile "maps/desert_tileset.json"
                >>= (decodeJsonTileset >>> errorEither)

failMaybe :: forall a. Maybe a -> Aff a
failMaybe (Just a) = pure a
failMaybe Nothing = throwError (error "Failed to get property")

-- | Runs a effect, then tests if the field matches
-- | Used to load a file off disk and test a property
testField :: forall a b . Eq a =>
                            Show a =>
                            Effect b
                            -> String 
                            -> (b -> a) 
                            -> a 
                            -> TestSuite
testField load name field expected = 
  test name do
    (field) <$> (liftEffect load) >>= Assert.equal expected