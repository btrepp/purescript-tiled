module Test.Main where

import Prelude

import Data.Tiled.Raw.Tileset.Unit (tilesetSuite)
import Effect (Effect)
import Test.Tiled.Map (mapSuite)
import Test.Unit.Main (runTest)

main :: Effect Unit
main = 
  runTest do
    tilesetSuite
    mapSuite
 