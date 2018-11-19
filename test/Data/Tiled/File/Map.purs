module Test.Data.Tiled.File.Map (mapSuite) where
import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap,wrap)
import Data.List (List,(:))
import Data.Tiled.File.Map (Map, Tileset(..), externalTileSets)
import Data.Tiled.File.Map.Layer (Layer, Type(..))
import Data.Tiled.File.Map.Layer.Tile (Data(..))
import Data.Tiled.File.Map.Layer.Tile as Ti
import Data.Tiled.File.Map.Orientation (Orientation(..))
import Data.Tiled.File.Map.RenderOrder (RenderOrder(..))
import Effect.Aff (Aff)
import Test.Tiled.Util as T
import Test.Unit (TestSuite, suite, testSkip, test)
import Test.Unit.Assert as Assert
import Data.Monoid (mempty)

isTile :: Type  -> Boolean
isTile (TileLayer _) = true
isTile _ = false


tile :: TestSuite
tile =
    suite "index 0" do
        test "gid" do
            val <- item
            Assert.equal 30 val.gid
        test "flipx" do
            val <- item
            Assert.equal false val.flipX
        test "flipy" do
            val <- item
            Assert.equal false val.flipY
        test "flipdiag" do
            val <- item
            Assert.equal false val.flipDiagonal
    where
       lay :: Aff Layer
       lay =  (flip Array.index 0 ) 
                <$>_.layers  
                <$> unwrap 
                <$> T.desertMap
                >>= T.failMaybe
       item :: Aff _
       item = getFirst 
              <$> _.type 
              <$> unwrap
              <$> lay
              >>= T.failMaybe
              >>= (getT >>> pure)
       getFirst :: Type -> Maybe Data
       getFirst (TileLayer (Ti.TileLayer x)) = Array.index x.data 0
       getFirst _ = Nothing

       getT :: Data -> _
       getT (Data x) = x

layer ::  TestSuite 
layer = 
    suite "index 0" do
        testSkip "chunks" (pure unit)
    --    testField "compression" _.compression Nothing
      --  testField "data" (_.data>>>isComp) true
      --  testField "encoding" _.encoding Base64
        testField "height" _.height 40
        testField "id" _.id 1
        testField "name" _.name "Ground"
        testSkip "objects" (pure unit)
        testField "opacity" _.opacity 1
        testField "type" (_.type>>>isTile) true
        testField "visible" _.visible true
        testField "width" _.width 40
        testField "x" _.x 0
        testField "y" _.y 0
        suite "tile" do
            tile
    where 
       item :: Aff Layer
       item =  (flip Array.index 0 ) 
                <$>_.layers  
                <$> unwrap 
                <$> T.desertMap
                >>= T.failMaybe
       testField :: forall a b . Eq b => Show b => Newtype Layer a =>
                        String -> (a->b) -> b -> TestSuite
       testField name acc exp = 
            T.testField item name acc exp

tileset :: TestSuite
tileset =
    suite "index 0" do
        testField "source" (_.source) "desert_tileset.json"
        testField "gid" (_.firstgid) 1
        
    where 
        item ::Aff Tileset
        item = (flip Array.index 0)
                <$> _.tileSets
                <$> unwrap
                <$> T.desertMap
                >>= T.failMaybe
        asExt = ext <$> item >>= T.failMaybe
        ext (Embedded _) = Nothing
        ext (External e) = Just e

        testField :: forall b . Eq b => Show b =>
                        String -> 
                        ({source::String ,firstgid::Int} -> b) -> b -> TestSuite
        testField name acc exp = 
            test name do
                x <- asExt
                Assert.equal exp (acc x)



desert :: TestSuite
desert = 
    suite "desert" do
        testField "height" _.height 40
        testField "width"  _.width 40
        testField "nextlayerid" _.nextLayerId 2
        testField "nextobjectid" _.nextObjectId 1
        testField "infinite" _.infinite false
        testField "renderorder" _.renderOrder RightDown
        testField "tiledversion" _.tiledVersion "1.2.0"
        testField "tileheight" _.tileHeight 32
        testField "tilewidth" _.tileWidth 32
        testField "version" _.version 1.2
        testField "layer count" (_.layers >>> Array.length) 1
        testField "orientation" _.orientation Orthoganal
        testField "external paths" (wrap >>> externalTileSets) expectedPaths
        suite "layers" do
            layer
        suite "tileset" do
            tileset
    where 
        testField :: forall a b . Show b => Eq b =>  Newtype Map a => 
                            String -> (a->b) -> b -> TestSuite
        testField name acc exp = 
            T.testField T.desertMap name acc exp 

        expectedPaths :: List String
        expectedPaths = 
            "desert_tileset.json" : mempty

mapSuite :: TestSuite
mapSuite = 
    suite "Map" 
        do desert 


