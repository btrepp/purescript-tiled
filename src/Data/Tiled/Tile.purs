module Data.Tiled.Tile 
    (Tile)
    where

import Data.Tiled.Texture (Texture)    

-- | A sprite is something we can render
-- | It consists of a base texture to render
-- | and positions
type Tile = 
    { height :: Int
    , width :: Int
    , x :: Int
    , y :: Int
    , texture :: Texture
    , flipX :: Boolean
    , flipY :: Boolean
    }