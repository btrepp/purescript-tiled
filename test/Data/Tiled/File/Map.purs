module Test.Data.Tiled.File.Map (mapSuite) where
import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Data.Tiled.File.Map (Map)
import Data.Tiled.File.Map.Layer (Layer)
import Data.Tiled.File.Map.Layer.Data (Data(..))
import Data.Tiled.File.Map.Layer.Encoding (Encoding(..))
import Data.Tiled.File.Map.Layer.Type (Type(..))
import Data.Tiled.File.Map.Orientation (Orientation(..))
import Data.Tiled.File.Map.RenderOrder (RenderOrder(..))
import Effect.Aff (Aff)
import Test.Tiled.Util as T
import Test.Unit (TestSuite, suite)

layer0 :: Aff Layer -> TestSuite 
layer0 file = 
    suite "[0]" do
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
       testField :: forall a b . Eq b => Show b => Newtype Layer a =>
                        String -> (a->b) -> b -> TestSuite
       testField name acc exp = 
            T.testField file name acc exp

       isComp :: Data -> Boolean
       isComp (Compressed _) = true
       isComp _ = false

desert :: Aff Map -> TestSuite
desert map = 
    suite "desert" do
        suite "layers" do
            layer0 (layer 0)
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
            T.testField map name acc exp 
        layer :: Int -> Aff Layer            
        layer ind = 
            ((l >>> flip Array.index ind) <$> T.desertMap)
            >>= T.failMaybe
            where 
                l :: Map -> Array Layer
                l m = _.layers $ unwrap m


mapSuite :: TestSuite
mapSuite = 
    suite "Map" 
        do desert T.desertMap


