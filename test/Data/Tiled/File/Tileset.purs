module Test.Data.Tiled.File.Tileset (tilesetSuite) where

import Prelude

import Data.Tiled.File.Tileset (Tileset, TilesetRecord)
import Test.Unit (TestSuite, suite, test)
import Effect.Aff (Aff)
import Test.Unit.Assert as Assert
import Data.Newtype(unwrap)
import Test.Tiled.Util (loadJsonFile)

desertSuite :: TestSuite
desertSuite = 
  suite "desert" do
      testField "columns" _.columns 8
      testField "image"  _.image "tmw_desert_spacing.png"
      testField "imageheight" _.imageHeight 199
      testField "imagewidth" _.imageWidth  265
      testField "margin" _.margin 1
      testField "name" _.name "Desert"
      testField "spacing" _.spacing 1
      testField "terrains" _.terrains []
      testField "tilecount" _.tileCount 48
      testField "tiledversion" _.tiledVersion "1.2.0"
      testField "tileheight" _.tileHeight 32
  where 
    load ::  Aff Tileset
    load =  loadJsonFile "maps/desert_tileset.json"
    testField :: forall a . Eq a =>
                             Show a =>
                             String 
                             -> (TilesetRecord-> a) 
                             -> a 
                             -> TestSuite
    testField name field expected = 
        test name do
          (unwrap >>> field) <$> load >>= Assert.equal expected

tilesetSuite :: TestSuite
tilesetSuite  =  
  suite "Tileset" do
    desertSuite

