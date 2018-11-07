module Data.Tiled.Raw.Tileset.Unit (tilesetSuite) where

import Prelude

import Data.Tiled.Raw.Tileset (Tileset(..))
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert as Assert
import Test.Tiled.Util (loadJsonFile)

desertSuite :: TestSuite
desertSuite = 
  suite "desert" do
    test "parsed" do
      actual >>= Assert.equal expected

  where actual =  loadJsonFile "maps/desert_tileset.json"
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
