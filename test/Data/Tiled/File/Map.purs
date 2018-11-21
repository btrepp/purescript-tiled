module Test.Data.Tiled.File.Map (mapSuite) where
import Prelude
import Data.Map as Map
import Data.Tiled.File.Map (Map)
import Data.Tiled.RenderOrder (RenderOrder(..))
import Test.Tiled.Util (testField)
import Test.Tiled.Util as T
import Test.Unit (TestSuite, suite)

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
        -- test' "layer count" (_.layers >>> Array.length) 1
        --test' "orientation" _.orientation Orthoganal
        --test' "external paths" externalTileSets expectedPaths 
        
        {-suite "layers" do
            suite "index 0" do
                test' "height" (Just 40) $ preview (_layer 0 <<< _height)
                test' "id" (Just 1) $ preview (_layer 0 <<< _id)
                test' "name" (Just "Ground") $ preview (_layer 0 <<< _name)
                test' "opacity" (Just 1) $ preview (_layer 0 <<< _opacity)
                test' "type" (true) $ preview (_layer 0 <<< _type) >>>  isTile
                test' "visibile" (Just true) $ preview (_layer 0 <<< _visible) 
                test' "width" (Just 40) $ preview (_layer 0 <<< _width)
                test' "x" (Just 0) $ preview (_layer 0 <<< _x)
                test' "y" (Just 0) $ preview (_layer 0 <<< _y)
            suite "tile" do
                suite "index 0" do
                    test' "gid" (Just 30) $ preview (_tile 0 0 <<< _gid)
                    test' "flipx" (Just false) $ preview (_tile 0 0 <<< _flipX)
                    test' "flipy" (Just false) $ preview (_tile 0 0 <<< _flipY)
                    test' "flipdiag" (Just false) 
                        $ preview (_tile 0 0 <<< _flipDiagonal)
                    pure unit
        suite "tileset" do
            suite "index 0" do
                test' "source" (Just "desert_tileset.json")  
                        $ preview (_source 0)
                test' "gid" (Just 1) $ preview (_firstgid 0)
                -}
    where 
        test' :: forall b . Show b => Eq b => 
                            String -> (Map ->  b) -> b-> TestSuite
        test' name acc exp  = testField T.desertMap name acc exp

        expectedPaths :: Map.Map Int String
        expectedPaths = 
            Map.singleton 1 "desert_tileset.json"

mapSuite :: TestSuite
mapSuite = 
    suite "Map" 
        do desert 

