module Test.Data.Tiled.File.Map (mapSuite) where
import Prelude

import Data.Array as Array
import Data.Newtype (class Newtype, unwrap)
import Data.Tiled.File.Map (Map)
import Data.Tiled.File.Map.Layer (Layer,Type(..))
import Data.Tiled.File.Map.Orientation (Orientation(..))
import Data.Tiled.File.Map.RenderOrder (RenderOrder(..))
import Effect.Aff (Aff)
import Test.Tiled.Util as T
import Test.Unit (TestSuite, suite,testSkip)

isTile :: Type  -> Boolean
isTile (Tile _) = true
isTile _ = false

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
        suite "layers" do
            layer
    where 
        testField :: forall a b . Show b => Eq b =>  Newtype Map a => 
                            String -> (a->b) -> b -> TestSuite
        testField name acc exp = 
            T.testField T.desertMap name acc exp 

mapSuite :: TestSuite
mapSuite = 
    suite "Map" 
        do desert 


