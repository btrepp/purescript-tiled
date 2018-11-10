module Data.Tiled.File.Property where

import Prelude
import Data.Newtype (wrap,class Newtype)
import Data.Argonaut (class DecodeJson, decodeJson, (.?))
type PropertyRecord = 
    { name :: String
      , ptype :: String
      , value :: String }
newtype Property = Property PropertyRecord
derive instance newtypeProperty :: Newtype Property _
instance decodeJsonProperty :: DecodeJson Property where
    decodeJson js = do
        o <- decodeJson js
        name <- o .? "name"
        ptype <- o .? "type"
        value <- o .? "value"
        pure $ wrap {
            name,value,ptype
        }