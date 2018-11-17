module Test.Data.Tiled.File.Tileset (tilesetSuite) where

import Prelude

import Data.Array as Array
import Data.Tiled.File.Tileset (Tileset)
import Test.Unit (TestSuite, suite)
import Data.Newtype(class Newtype)
import Test.Tiled.Util as T

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
      testField "count" (_.terrains >>> Array.length) 4
      --suite "terrains" do
        --testTerrain "desert" 0 "Desert" 29
        --testTerrain "brick" 1 "Brick" 9
        --testTerrain "cobblestone" 2 "Cobblestone" 33
        --testTerrain "dirt" 3 "Dirt" 14
      testField "tilecount" _.tileCount 48
      testField "tiledversion" _.tiledVersion "1.2.0"
      testField "tileheight" _.tileHeight 32
      suite "tiles" do
        testField "count" (_.tiles >>> Array.length) 48
        --testTile 0 0 (tuple4 0 0 0 1)
        --testTile 1 1 (tuple4 0 0 1 1)
        --testTile 2 2 (tuple4 0 0 1 0)
        --testTile 3 3 (tuple4 3 3 3 0)
        --testTile 4 4 (tuple4 3 3 0 3)
        --testTile 5 5 (tuple4 0 0 0 3)
      testField "tilewidth" _.tileWidth 32
      testField "type" _.typeTileset "tileset"
      testField "version" _.version 1.2

  where 
    testField :: forall a b . Eq b => Show b => Newtype Tileset a =>
                        String -> (a->b) -> b -> TestSuite
    testField name acc exp = 
            T.testField T.desertTileset name acc exp

tilesetSuite :: TestSuite
tilesetSuite  =  
  suite "Tileset" do
    desertSuite

