module Test.Data.Tiled.File.Layer 
            (layerSuite) where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype,unwrap )
import Data.Tiled.File.Layer (Encoding(..), Layer,Type(..))
import Data.Tiled.File.Data (Data(..))
import Effect.Aff (Aff)
import Test.Tiled.Util as T
import Test.Unit (TestSuite, suite)
desert :: TestSuite 
desert = 
    suite "desert[0]" do
        testField "compression" _.compression Nothing
        testField "data" (_.data>>>isComp) true
        testField "encoding" _.encoding Base64
        testField "height" _.height 40
        testField "id" _.id 1
        testField "name" _.name "Ground"
        testField "opacity" _.opacity 1
        testField "type" _.type TileLayer
        testField "visible" _.visible true
        testField "width" _.width 40
        testField "x" _.x 0
        testField "y" _.y 0

    where 
        file :: Aff Layer 
        file = (unwrap >>> _.layers  >>> flip Array.index 0 )
                <$> T.desertMap 
                >>= T.failMaybe
        testField :: forall a b . Eq b => Show b => Newtype Layer a =>
                        String -> (a->b) -> b -> TestSuite
        testField name acc exp = 
            T.testField file name acc exp

        isComp :: Data -> Boolean
        isComp (Compressed _) = true
        isComp _ = false


layerSuite :: TestSuite        
layerSuite = 
    suite "layer" do
        desert
