module Test.Tiled.Map (mapSuite) where

import Prelude

import Data.Newtype (unwrap)
import Data.Tiled.Raw.Map (Map,MapRecord, RenderOrder(..))
import Effect.Aff (Aff)
import Test.Tiled.Util (loadJsonFile)
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert as Assert


mapSuite :: TestSuite
mapSuite = 
    suite "map" do
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
    where 
        load ::  Aff Map
        load =  loadJsonFile "maps/desert.json"
        testField :: forall a . Eq a =>
                                 Show a =>
                                 String 
                                 -> (MapRecord-> a) 
                                 -> a 
                                 -> TestSuite
        testField name field expected = 
            test name do
              (unwrap >>> field) <$> load >>= Assert.equal expected