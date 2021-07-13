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
  , keys
  , GameState (..)
  , AstronautGraphics (..)
  , Graphics (..)
  , Heading (..)
  , PlayerState (..)
  , PlrDirection (..)
  ) where

import qualified Data.Map                           as Map
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

keys ∷ GameKeys
keys =
  Map.fromList [("KeyLeft", False), ("KeyRight", False), ("KeySpace", False)]

data Heading
  = Left
  | Right
  deriving (Show, Eq)

data PlrDirection
  = West
  | East
  | North
  | South
  | None
  deriving (Show)

data AstronautGraphics =
  AstronautGraphics
    { left  ∷ [Picture]
    , right ∷ [Picture]
    }
  deriving (Show)

data Graphics =
  Graphics
    { tiles     ∷ [Picture]
    , astronaut ∷ AstronautGraphics
    }
  deriving (Show)

data PlayerState
  = Idle
  | Walk
  deriving (Show, Eq)

type GameKeys = Map.Map String Bool

data GameState =
  GameState
    { ls          ∷ [Tile]
    , assets      ∷ Graphics
    , heading     ∷ Heading
    , velX        ∷ Int
    , spriteCount ∷ Int
    , velY        ∷ Int
    , state       ∷ PlayerState
    , direction   ∷ PlrDirection
    , pos         ∷ Position
    , keyEvents   ∷ GameKeys
    }
  deriving (Show)
