module Constants
  ( background
  , forgroundColor
  , tileSize
  , Position
  , TileType
  , Tile
  , astronautIdle
  , astronautWalk
  , astronautShot
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
  , obstacles
  , speed
  , gravity
  , projectile
  , Shot
  ) where

import qualified Data.Map                           as Map
import           Graphics.Gloss

forgroundColor ∷ Color
forgroundColor = makeColorI 15 147 4 255

background ∷ Color
background = black -- makeColorI 199 240 216 255

tileSize ∷ Int
tileSize = 48

speed ∷ Float
speed = 2.0

gravity ∷ Float
gravity  = 0.4

type Position = (Int, Int)

type TileType = String

type Tile = (Position, TileType)

astronautIdle = [1, 2]

astronautWalk = [4, 5]

astronautShot :: Int
astronautShot = 2

astronautJumpCompress = [6]

astronautJumpAscend = [7]

astronautJumpDescend = [8]

astronautJumpLand = [9]

projectile = [0]

keys ∷ GameKeys
keys =
  Map.fromList [("MoveRight", False), ("MoveLeft", False), ("Jump", False)]

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
    , other ∷ [Picture]
    }
  deriving (Show)

data PlayerState
  = Idle
  | Jump
  | Walk
  | Shot
  deriving (Show, Eq)

type GameKeys = Map.Map String Bool

type Shot = ((Int, Int), Bool, Heading)

data GameState =
  GameState
    { ls          ∷ [Tile]
    , assets      ∷ Graphics
    , isShooting  :: Bool
    , shots       ∷ [Shot]
    , heading     ∷ Heading
    , velX        ∷ Float
    , spriteCount ∷ Int
    , velY        ∷ Float
    , state       ∷ PlayerState
    , direction   ∷ PlrDirection
    , pos         ∷ Position
    , keyEvents   ∷ GameKeys
    }
  deriving (Show)

obstacles ∷ [String]
obstacles = map show $ [0..6] ++ [16..22] ++ [32..36] ++ [48..52]
