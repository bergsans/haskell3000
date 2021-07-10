module GameLoop
  ( gameLoop
  , formatLevel
  , createLevel
  ) where

import           Constants
import           Data.Char                          (toUpper)
import qualified Data.Map                           as Map
import           Data.Maybe
import           Graphics.Gloss
import           Graphics.Gloss.Interface.Pure.Game
import           InitGame
import           Text

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

type GameKeys = Map.Map String Bool

keys ∷ GameKeys
keys =
  Map.fromList [("KeyLeft", False), ("KeyRight", False), ("KeySpace", False)]

getDir ∷ GameState → Heading
getDir = heading

getSpriteCount ∷ Int → Int
getSpriteCount c
  | c < 30 = 0
  | c >= 30 && c < 60 = 1
  | otherwise = 0

getAstronautImg ∷ GameState → Picture
getAstronautImg gs
  | state gs == Idle =
    if heading gs == GameLoop.Left
      then (left $ astronaut $ getAssets gs) !!
           (getSpriteCount (spriteCount gs))
      else (right $ astronaut $ getAssets gs) !!
           (getSpriteCount (spriteCount gs))
  | state gs == Walk =
    if heading gs == GameLoop.Left
      then (left $ astronaut $ getAssets gs) !!
           (4 + (getSpriteCount (spriteCount gs)))
      else (right $ astronaut $ getAssets gs) !!
           (4 + (getSpriteCount (spriteCount gs)))
  | otherwise =
    if heading gs == GameLoop.Left
      then (left $ astronaut $ getAssets gs) !! 1
      else (right $ astronaut $ getAssets gs) !! 1

drawAstronaut ∷ GameState → [Picture]
drawAstronaut gs =
  [uncurry translate (fromIntegral x, fromIntegral y) $ getAstronautImg gs]
  where
    (x, y) = pos gs

drawTile ∷ Tile → Picture → Picture
drawTile ((x, y), t) = uncurry translate (fromIntegral x, fromIntegral y)

getAssets ∷ GameState → Graphics
getAssets = assets

f k gs =
  if Map.lookup k (keyEvents gs) == Just True
    then "TRUE"
    else "FALSE"
    -- show $ map toUpper k ++

render ∷ GameState → Picture
render gs =
  pictures $
  [drawTile cell (tiles graphics !! read (snd cell)) | cell ← ls gs] ++
  printText (0, 0) keys ++ drawAstronaut gs
  where
    graphics = getAssets gs
    keys = concatMap (\x → f x gs) ["KeyLeft", "KeyRight"]

handleKeys ∷ Event → GameState → GameState
handleKeys (EventKey (SpecialKey KeyLeft) Down _ _) gs =
  gs
    { keyEvents = Map.insert "KeyLeft" True (keyEvents gs)
    , velX = -2
    , direction = West
    , heading = GameLoop.Left
    , state = Walk
    }
handleKeys (EventKey (SpecialKey KeyLeft) Up _ _) gs =
  gs
    { keyEvents = Map.insert "KeyLeft" False (keyEvents gs)
    , velX = 0
    , direction = None
    , heading = GameLoop.Left
    , state = Idle
    }
handleKeys (EventKey (SpecialKey KeyRight) Down _ _) gs =
  gs
    { keyEvents = Map.insert "KeyRight" True (keyEvents gs)
    , velX = 2
    , direction = West
    , heading = GameLoop.Right
    , state = Walk
    }
handleKeys (EventKey (SpecialKey KeyRight) Up _ _) gs =
  gs
    { keyEvents = Map.insert "KeyRight" False (keyEvents gs)
    , velX = 0
    , direction = None
    , heading = GameLoop.Right
    , state = Idle
    }
handleKeys _ gs = gs

updateX gs =
  if velX gs /= 0
    then ((fst $ pos gs) + (velX gs), snd $ pos gs)
    else pos gs

updateSpriteCount gs
  | state gs == Idle =
    if (spriteCount gs) + 1 >= 60
      then 0
      else (spriteCount gs) + 1
  | state gs == Walk =
    if (spriteCount gs) + 4 >= 60
      then 0
      else (spriteCount gs) + 4
  | otherwise = 0

update ∷ Float → GameState → GameState
update _ gs = gs {pos = updateX gs, spriteCount = updateSpriteCount gs}

initialState ∷ GameState
initialState =
  GameState
    { ls = []
    , assets =
        Graphics
          {tiles = [], astronaut = AstronautGraphics {left = [], right = []}}
    , heading = GameLoop.Right
    , state = Idle
    , velX = 0
    , velY = 0
    , pos = (0, 0)
    , spriteCount = 0
    , keyEvents = keys
    , direction = East
    }

--    , keyEvents = keys
gameLoop ∷ IO ()
gameLoop = do
  rawLevel ← readFile $ head levelData
  let level = createLevel rawLevel
  tileImgs ← traverse loadBMP bmpsTiles
  astronautLeft ← traverse loadBMP bmpsAstronautLeft
  astronautRight ← traverse loadBMP bmpsAstronautRight
  let astronautImgs =
        AstronautGraphics {left = astronautLeft, right = astronautRight}
  let graphics = Graphics {tiles = tileImgs, astronaut = astronautImgs}
  let state = initialState {ls = level, assets = graphics}
  play window background fps state render handleKeys update
