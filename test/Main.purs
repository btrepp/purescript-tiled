module Test.Main where

import Prelude

import Test.Data.Tiled.File.Tileset (tilesetSuite)
import Test.Data.Tiled.File.Map (mapSuite)
import Effect (Effect)
import Test.Unit.Main (runTest)

main :: Effect Unit
main = 
  runTest do
    tilesetSuite
    mapSuite
 