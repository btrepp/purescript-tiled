module Data.Tiled.File.Map.Layer.Tile where

import Prelude

import Control.Monad.Error.Class (throwError)
import Data.Argonaut ((.?), (.??))
import Data.Argonaut.Decode.Class (class DecodeJson, decodeJson)
import Data.ArrayBuffer.DataView as V
import Data.ArrayBuffer.Typed (asInt32Array, toIntArray)
import Data.Int.Bits as Bit
import Data.ArrayBuffer.Types (ArrayBuffer)
import Data.Base64 as B
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype,wrap)
import Effect.Exception (message)
import Pako as Pako

newtype Data = Data {
    flipX :: Boolean
    ,flipY :: Boolean
    ,flipDiagonal :: Boolean
    ,gid :: Int
}

fromInt :: Int -> Data
fromInt i = Data {
    flipX,flipY,flipDiagonal,gid
    }
    where 
        flipX = (Bit.zshr i 31) > 0  
        flipY = (Bit.zshr (Bit.shl i 1) 31) > 0
        flipDiagonal = (Bit.zshr (Bit.shl i 2) 31) > 0
        gid = Bit.zshr (Bit.shl i 3 ) 3
    

data Algorithm = Zlib | Gzip 
data Encoding = Base64 
data Compression = Uncompressed | Compressed Algorithm

newtype TileLayer = TileLayer
    { data :: Array Data
      ,encoding :: Maybe Compression
    }
derive instance newtypeTileLayer :: Newtype TileLayer _

instance decodeJsonEncoding :: DecodeJson Encoding where
    decodeJson js = do
        s <- decodeJson js 
        case s of
          "base64" -> pure Base64
          x -> throwError $ x <> " is not a valid encoding"
instance decodeJsonAlgorithm :: DecodeJson Algorithm where              
    decodeJson js = do
        s <- decodeJson js
        case s of
            "gzip" -> pure Gzip
            "zlib" -> pure Zlib
            x -> throwError $ x <> " is not a valid compression"

instance decodeJsonTile :: DecodeJson TileLayer where
    decodeJson js = do
        o <- decodeJson js
        encoding <- o .?? "encoding"
        compression <- o .?? "compression"
        

        --- This has gotten a bit procedural
        --- and here be dragons-y
        --- should refactor
        case encoding,compression of
            Nothing , _ -> do
              data' <- o .? "data"
              pure $ wrap { encoding : Nothing , data: convert data'}
            Just Base64, Nothing -> do
              data' <- o .? "data"
                         >>= base64Decode
              pure $ wrap { encoding : Just Uncompressed
                            , data : convert $ extract data'}
            Just Base64, Just Zlib  -> do
                d <- o .? "data"
                     >>= base64Decode
                     >>= zlib
                pure $ wrap $ {
                        encoding : Just $ Compressed Zlib
                        , data : convert $ extract d
                }
            Just Base64, Just Gzip -> do
                d <- o .? "data"
                     >>= base64Decode
                     >>= gzip
                pure $ wrap $ {
                        encoding : Just  $ Compressed Gzip
                        , data : convert $ extract d
                }                             
        where                        
            base64Decode :: String -> Either String (ArrayBuffer)
            base64Decode s =
                case B.decodeBase64 $ B.Base64 s of
                    Just arr -> pure arr 
                    _ -> throwError "Unable to decode bas64"

            zlib :: ArrayBuffer -> Either String (ArrayBuffer)
            zlib r= 
                case Pako.inflate r of 
                    Left e -> throwError $ message e
                    Right x -> pure x

            gzip :: ArrayBuffer -> Either String (ArrayBuffer)
            gzip r= 
                -- Pako seems to autodect inflat things
                -- This is here as a punt and is.. untested
                case Pako.inflate r of 
                    Left e -> throwError $ message e
                    Right x -> pure x

            extract :: ArrayBuffer -> Array Int
            extract x =  toIntArray $ asInt32Array $ V.whole x

            convert :: Array Int -> Array Data
            convert x = map fromInt x