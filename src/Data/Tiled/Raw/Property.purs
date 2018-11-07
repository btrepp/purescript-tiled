module Data.Tiled.Raw.Property where

newtype Property = Property 
        { name:: String
          , value:: String
          , type:: String}