module Test.Data.Tiled.File.Map (mapSuite) where
import Prelude

import Data.Array as Array
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Tiled.File.Map (Map, Layer(..), TileLayer, Data(..), externalTilesets)
import Data.Tiled.Orientation (Orientation(..))
import Data.Tiled.RenderOrder (RenderOrder(..))
import Test.Tiled.Util (testField)
import Test.Tiled.Util as T
import Test.Unit (TestSuite, suite)
import Type.Data.Boolean (kind Boolean)

desert :: TestSuite
desert = 
    suite "desert" do
        test' "height"  _.height  40
        test' "width" _.width 40
        test' "nextlayerid" _.nextLayerId 2 
        test' "nextobjectid" _.nextObjectId 1
        test' "infinite" _.infinite false
        test' "renderorder"  _.renderOrder RightDown
        test' "tiledversion" _.tiledVersion "1.2.0"
        test' "tileheight" _.tileHeight 32
        test' "tilewidth" _.tileWidth 32
        test' "version" _.version  1.2
        test' "layer count" (_.layers >>> Array.length) 1
        test' "orientation" _.orientation Orthoganal
        test' "external paths" externalTilesets expectedTilesets 

        suite "layers" do
            suite "index 0" do
                test' "height" (layer 0 >>> map _.height) (pure 40) 
                test' "id" (layer 0 >>> map _.id) (pure 1)
                test' "name" (layer 0 >>> map _.name) (pure "Ground")
                test' "opacity" (layer 0>>> map _.opacity) (pure 1)
                test' "visible" (layer 0>>> map _.visible) (pure true)
                test' "width" (layer 0>>> map _.width) (pure 40)
                test' "x" (layer 0>>> map _.x) (pure 0)
                test' "y" (layer 0>>> map _.y) (pure 0)

            suite "tile" do
                suite "index 0" do
                    let tile = layer 0 >>> map _.data 
                                       >>> map isArrayString
                    test' "value" (tile ) (pure true)
    where 

        isArrayString :: Data -> Boolean
        isArrayString (DataString x) = true
        isArrayString _ = false

        layer :: Int -> Map -> Maybe TileLayer
        layer ix map = _.layers map # flip Array.index ix >>= unlayer
    
        unlayer :: Layer -> Maybe TileLayer
        unlayer (TileLayer l) = pure l
    
        test' :: forall b . Show b => Eq b => 
                            String -> (Map ->  b) -> b-> TestSuite
        test' name acc exp  = testField T.desertMap name acc exp

        expectedTilesets :: Map.Map Int String
        expectedTilesets = 
            Map.singleton 1 "desert_tileset.json"

mapSuite :: TestSuite
mapSuite = 
    suite "Map" 
        do desert 

