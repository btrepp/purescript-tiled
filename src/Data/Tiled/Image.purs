module Data.Tiled.Image
    (Image)
    where

import Web.File.Blob (Blob)

newtype Image = Image Blob