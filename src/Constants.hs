module Constants
  ( background
  , forgroundColor
  , tileSize
  , Position
  , TileType
  , Tile
  ) where

import           Graphics.Gloss

forgroundColor ∷ Color
forgroundColor = makeColorI 15 147 4 255

background ∷ Color
background = black -- makeColorI 199 240 216 255

tileSize ∷ Int
tileSize = 48

type Position = (Int, Int)

type TileType = String

type Tile = (Position, TileType)
