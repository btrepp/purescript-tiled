module Data.Tiled.File.Map.Layer.Tile where

import Prelude

import Control.Monad.Error.Class (throwError)
import Data.Argonaut ((.?), (.??))
import Data.Argonaut.Decode.Class (class DecodeJson, decodeJson)
import Data.ArrayBuffer.DataView as V
import Data.ArrayBuffer.Typed (asInt8Array, toIntArray)
import Data.ArrayBuffer.Types (ArrayBuffer)
import Data.Base64 as B
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
type Gid = Int -- TODO, unsigned int

data Algorithm = Zlib | Gzip 
data Encoding = Base64 
data Compression = Uncompressed | Compressed Algorithm

newtype Tile = Tile 
    { data :: Array Gid
      ,encoding :: Maybe Compression
    }
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

instance decodeJsonTile :: DecodeJson Tile where
    decodeJson js = do
        o <- decodeJson js
        encoding <- o .?? "encoding"
        compression <- o .?? "compression"
        
        case encoding,compression of
            Nothing , _ -> do
              data' <- o .? "data"
              pure $ Tile { encoding : Nothing , data: data'}
            Just Base64, Nothing -> do
              data' <- o .? "data"
                         >>= base64Decode
              pure $ Tile { encoding : Just Uncompressed
                            , data : extract data'}
            Just Base64, Just Zlib  -> do
                d <- o .? "data"
                     >>= base64Decode
                     >>= zlib
                pure $ Tile $ {
                        encoding : Just $ Compressed Zlib
                        , data : extract d
                }
            Just Base64, Just Gzip -> do
                d <- o .? "data"
                     >>= base64Decode
                     >>= gzip
                pure $ Tile $ {
                        encoding : Just  $ Compressed Gzip
                        , data : extract d
                }                             
        where                        
            base64Decode :: String -> Either String (ArrayBuffer)
            base64Decode s =
                case B.decodeBase64 $ B.Base64 s of
                    Just arr -> pure arr 
                    _ -> throwError "Unable to decode bas64"

            zlib :: ArrayBuffer -> Either String (ArrayBuffer)
            zlib _ = throwError "Zlib not supported"

            gzip :: ArrayBuffer -> Either String (ArrayBuffer)
            gzip _ = throwError "GZIP Not supported"

            extract :: ArrayBuffer -> Array Int
            extract x = toIntArray $ asInt8Array $ V.whole x