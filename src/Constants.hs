module Constants
  ( background
  , forgroundColor
  , tileSize
  , Position
  , TileType
  , Tile
  , astronautIdle
  , astronautWalk
  , astronautShoot
  , astronautJumpCompress
  , astronautJumpAscend
  , astronautJumpDescend
  , astronautJumpLand
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

astronautIdle = [1, 2]

astronautWalk = [4, 5]

astronautShoot = [3]

astronautJumpCompress = [6]

astronautJumpAscend = [7]

astronautJumpDescend = [8]

astronautJumpLand = [9]
