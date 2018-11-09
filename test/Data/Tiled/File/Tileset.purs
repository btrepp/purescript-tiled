module Test.Data.Tiled.File.Tileset (tilesetSuite) where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Tiled.File.Tileset (Tileset, TilesetRecord,Terrain, TerrainRecord)
import Effect.Aff (Aff, throwError)
import Test.Tiled.Util (loadJsonFile)
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert as Assert

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
      suite "terrains" do
        testField "count" (_.terrains >>> Array.length) 4
        testTerrain "desert" 0 "Desert" 29
        testTerrain "brick" 1 "Brick" 9
        testTerrain "cobblestone" 2 "Cobblestone" 33
        testTerrain "dirt" 3 "Dirt" 14
      testField "tilecount" _.tileCount 48
      testField "tiledversion" _.tiledVersion "1.2.0"
      testField "tileheight" _.tileHeight 32
      testField "tilewidth" _.tileWidth 32
      testField "type" _.typeTileset "tileset"
      testField "version" _.version 1.2

  where 
    load ::  Aff Tileset
    load =  loadJsonFile "maps/desert_tileset.json"

    testUnsafeField :: forall a . Eq a =>
                             Show a =>
                             String 
                             -> (TilesetRecord-> Maybe a) 
                             -> a 
                             -> TestSuite
    testUnsafeField name field expected = 
        test name do
          (unwrap >>> field) <$> load >>= Assert.equal (pure expected)

    testField :: forall a .Eq a => Show a =>
                  String -> (TilesetRecord -> a) -> a -> TestSuite
    testField name field expected = 
      testUnsafeField name (field >>> pure) expected

    testTerrain:: String -> Int -> String -> Int -> TestSuite
    testTerrain name i tname ttile = 
      suite name do
        testUnsafeField "name" (item _.name) tname
        testUnsafeField "tile" (item _.tile) ttile
      where 
        item :: forall a. (TerrainRecord -> a) 
                 -> TilesetRecord 
                 -> Maybe a
        item prop x = (unwrap >>> prop) <$> Array.index x.terrains i




tilesetSuite :: TestSuite
tilesetSuite  =  
  suite "Tileset" do
    desertSuite

