module Test.Data.Tiled.File.Map (mapSuite) where
import Prelude
import Data.Newtype (class Newtype)
import Data.Tiled.File.Map (Map, Orientation(..), RenderOrder(..))
import Test.Unit (TestSuite, suite)
import Test.Tiled.Util as T 
import Data.Array as Array


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
    where 
        testField :: forall a b . Show b => Eq b =>  Newtype Map a => 
                            String -> (a->b) -> b -> TestSuite
        testField name acc exp = 
            T.testField T.desertMap name acc exp 

mapSuite :: TestSuite
mapSuite = 
    suite "map" do
        desert