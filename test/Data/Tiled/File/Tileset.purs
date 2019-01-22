module Test.Data.Tiled.File.Tileset (tilesetSuite) where

import Prelude
import Data.Array as Array
import Data.Maybe (fromMaybe)
import Data.Tiled.File.Tileset (Tileset)
import Test.Tiled.Util (testField)
import Test.Tiled.Util as T
import Test.Unit (TestSuite, suite)

desertSuite :: TestSuite
desertSuite = 
  suite "desert" do
      test' "columns" _.columns 8
      test' "image"  _.image "tmw_desert_spacing.png"
      test' "imageheight" _.imageHeight 199
      test' "imagewidth" _.imageWidth  265
      test' "margin" _.margin 1
      test' "name" _.name "Desert"
      test' "spacing" _.spacing 1
      test' "tilecount" _.tileCount 48
      test' "tiledversion" _.tiledVersion "1.2.0"
      test' "tileheight" _.tileHeight 32
      test' "tilewidth" _.tileWidth 32
      test' "type" _.type "tileset"
      test' "version" _.version 1.2
      suite "tiles" do
        test' "count" (_.tiles >>> map Array.length) (pure 48)
        suite "index 0" do
          let tile m = flip Array.index 0 $ fromMaybe mempty (_.tiles m)
          test' "id" (tile >>> map _.id) (pure 0)
          test' "terrain" (tile >>> map _.terrain) (pure [0,0,0,1])
      suite "terrains" do
        test' "count" (_.terrains >>> map Array.length) (pure 4)
        suite "index 0" do
          let terrain m = flip Array.index 0 $ fromMaybe mempty (_.terrains m)
          test' "name" (terrain >>> map _.name) (pure "Desert")
          test' "tile" (terrain >>> map _.tile) (pure 29)

  where 
    test' :: forall b . Show b => Eq b =>
                        String -> (Tileset -> b) -> b-> TestSuite
    test' name acc exp = testField T.desertTileset name acc exp 

tilesetSuite :: TestSuite
tilesetSuite  =  
  suite "Tileset" do
    desertSuite

