module Test.Data.Tiled.File.Tileset (tilesetSuite) where

import Prelude

import Data.Array as Array
import Data.Newtype (class Newtype,unwrap)
import Data.Tiled.File.Tileset (Tileset)
import Data.Tiled.File.Tileset.Tile (Tile)
import Data.Tiled.File.Tileset.Terrain (Terrain)
import Test.Tiled.Util as T
import Data.Tuple.Nested (tuple4)
import Test.Unit (TestSuite, suite)
import Effect.Aff (Aff)

tile :: TestSuite
tile = 
  suite "index 0" do
    testField "id" _.id 0
    testField "terrain" _.terrain (tuple4 0 0 0 1)

  where
    item :: Aff Tile
    item = (flip Array.index 0)
            <$> _.tiles
            <$> unwrap
            <$> T.desertTileset
            >>= T.failMaybe
    testField :: forall a b . Eq b => Show b => Newtype Tile a =>
                      String -> (a->b) -> b -> TestSuite
    testField name acc exp = 
      T.testField item name acc exp                      

terrain :: TestSuite
terrain = 
  suite "index 0" do
    testField "name" _.name "Desert"
    testField "tile" _.tile 29

  where
    item :: Aff Terrain
    item = (flip Array.index 0)
            <$> _.terrains
            <$> unwrap
            <$> T.desertTileset
            >>= T.failMaybe
    testField :: forall a b . Eq b => Show b => Newtype Terrain a =>
                      String -> (a->b) -> b -> TestSuite
    testField name acc exp = 
      T.testField item name acc exp                      


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
      testField "tilecount" _.tileCount 48
      testField "tiledversion" _.tiledVersion "1.2.0"
      testField "tileheight" _.tileHeight 32
      testField "tilewidth" _.tileWidth 32
      testField "type" _.typeTileset "tileset"
      testField "version" _.version 1.2
      testField "tile count" (_.tiles >>> Array.length) 48
      suite "tiles" do
        tile
      suite "terrains" do
        terrain

  where 
    testField :: forall a b . Eq b => Show b => Newtype Tileset a =>
                        String -> (a->b) -> b -> TestSuite
    testField name acc exp = 
            T.testField T.desertTileset name acc exp

tilesetSuite :: TestSuite
tilesetSuite  =  
  suite "Tileset" do
    desertSuite

