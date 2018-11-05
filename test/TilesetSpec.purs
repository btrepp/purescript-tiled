module Data.Tiled.Raw.Tileset.Unit where

import Prelude

import Data.Argonaut (Json)
import Data.Argonaut.Parser (jsonParser)
import Data.Either (Either(..))
import Data.Tiled.Raw.Tileset (Tileset, parseTileSet)
import Data.Tiled.Raw.Tileset as Tileset
import Effect.Aff (Aff, error, throwError)
import Test.Unit ( TestSuite, suite, test)
import Test.Unit.Assert as Assert

jsonText :: String 
jsonText = "{\"columns\":2}"

json :: Either String Json
json = jsonParser jsonText

tileset :: Aff Tileset
tileset = 
  case json of
  Left x -> throwError (error x)
  Right y -> case parseTileSet y of
             Left x -> throwError (error x)
             Right z -> pure z

tilesetSuite :: TestSuite
tilesetSuite  =  
  suite "tileset" do
    test "has correct columns" do
      (Tileset.Tileset tiles) <- tileset
      Assert.equal tiles.columns 2

