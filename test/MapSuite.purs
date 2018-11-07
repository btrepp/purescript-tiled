module Test.Tiled.Map (mapSuite) where

import Prelude

import Data.Newtype (unwrap)
import Data.Tiled.Raw.Map (Map)
import Effect.Aff (Aff)
import Test.Tiled.Util (loadJsonFile)
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert as Assert


mapSuite :: TestSuite
mapSuite = 
    suite "map" do
        testField "height" _.height 40
        testField "width" _.width 40

    where 
        load :: Aff Map
        load = loadJsonFile "maps/desert.json"
        actual = unwrap <$> load
        testField name field expected = 
            test name do
              field <$> actual >>= Assert.equal expected