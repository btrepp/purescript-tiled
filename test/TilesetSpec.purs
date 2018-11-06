module Data.Tiled.Raw.Tileset.Unit (tilesetSuite) where

import Prelude

import Data.Argonaut (decodeJson)
import Data.Argonaut.Parser (jsonParser)
import Data.Either (Either(..))
import Data.Tiled.Raw.Tileset (Tileset(..))
import Effect.Aff (Aff, error, throwError)
import Effect.Class (liftEffect)
import Node.Encoding (Encoding(..))
import Node.FS.Sync as FS
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert as Assert

loadTileSet :: String -> Aff Tileset
loadTileSet path = do
  file <- liftEffect $ FS.readTextFile ASCII path
  js <- json file
  case decodeJson js of
    Left x -> throwError (error x)
    Right x -> pure x
  where json text =
          case jsonParser text of
          Left x -> throwError (error x)
          Right x -> pure x

desertSuite :: TestSuite
desertSuite = 
  suite "desert" do
    test "parsed" do
      actual >>= Assert.equal expected

  where actual =  loadTileSet "maps/desert_tileset.json"
        columns = 8
        image = "tmw_desert_spacing.png"
        imageHeight = 199
        imageWidth = 265
        margin = 1
        name = "Desert"
        spacing = 1
        terrains = []
        tileCount = 48
        tiledVersion = "1.2.0"
        tileHeight = 32
        expected = Tileset {   
                     columns
                     , image
                     , imageHeight
                     , imageWidth
                     , margin
                     , name
                     , spacing 
                     , terrains
                     , tileCount
                     , tiledVersion
                     , tileHeight }

tilesetSuite :: TestSuite
tilesetSuite  =  
  suite "Tileset" do
    desertSuite
